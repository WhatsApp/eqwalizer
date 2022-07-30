/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

//! `LineIndex` maps flat `TextSize` offsets into `(Line, Column)`
//! representation.

// Adopted from https://github.com/rust-analyzer/rust-analyzer/blob/1d1da5ea8c35b3d731225292a6386e4c33907d7e/crates/ide_db/src/line_index.rs
use std::cmp::Ordering;
use std::iter;

use fxhash::FxHashMap;
use text_size::TextRange;
use text_size::TextSize;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LineIndex {
    /// Offset of the beginning of each line, zero-based
    pub(crate) newlines: Vec<TextSize>,
    /// List of offsets of utf16 surrogates on each line
    pub(crate) utf16_lines: FxHashMap<u32, Vec<Utf16Char>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LineCol {
    /// Zero-based
    pub line: u32,
    /// Zero-based
    pub col_utf16: u32,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct Utf16Char {
    /// Start offset of a character inside a line, zero-based
    pub(crate) start: TextSize,
    /// End offset of a character inside a line, zero-based
    pub(crate) end: TextSize,
}

impl Utf16Char {
    /// Returns the length in 8-bit UTF-8 code units.
    fn len(&self) -> TextSize {
        self.end - self.start
    }

    /// Returns the length in 16-bit UTF-16 code units.
    fn len_utf16(&self) -> usize {
        if self.len() == TextSize::from(4) {
            2
        } else {
            1
        }
    }
}

impl LineIndex {
    pub fn new(text: &str) -> LineIndex {
        let mut utf16_lines = FxHashMap::default();
        let mut utf16_chars = Vec::new();

        let mut newlines = vec![0.into()];
        let mut curr_row = 0.into();
        let mut curr_col = 0.into();
        let mut line = 0;
        for c in text.chars() {
            let c_len = TextSize::of(c);
            curr_row += c_len;
            if c == '\n' {
                newlines.push(curr_row);

                // Save any utf-16 characters seen in the previous line
                if !utf16_chars.is_empty() {
                    utf16_lines.insert(line, utf16_chars);
                    utf16_chars = Vec::new();
                }

                // Prepare for processing the next line
                curr_col = 0.into();
                line += 1;
                continue;
            }

            if !c.is_ascii() {
                utf16_chars.push(Utf16Char {
                    start: curr_col,
                    end: curr_col + c_len,
                });
            }

            curr_col += c_len;
        }

        // Save any utf-16 characters seen in the last line
        if !utf16_chars.is_empty() {
            utf16_lines.insert(line, utf16_chars);
        }

        LineIndex {
            newlines,
            utf16_lines,
        }
    }

    pub fn line_col(&self, offset: TextSize) -> LineCol {
        let line = partition_point(&self.newlines, |&it| it <= offset) - 1;
        let line_start_offset = self.newlines[line];
        let col = offset - line_start_offset;

        LineCol {
            line: line as u32,
            col_utf16: self.utf8_to_utf16_col(line as u32, col) as u32,
        }
    }

    pub fn offset(&self, line_col: LineCol) -> TextSize {
        //FIXME: return Result
        let col = self.utf16_to_utf8_col(line_col.line, line_col.col_utf16);
        self.newlines[line_col.line as usize] + col
    }

    pub fn lines(&self, range: TextRange) -> impl Iterator<Item = TextRange> + '_ {
        let lo = partition_point(&self.newlines, |&it| it < range.start());
        let hi = partition_point(&self.newlines, |&it| it <= range.end());
        let all = iter::once(range.start())
            .chain(self.newlines[lo..hi].iter().copied())
            .chain(iter::once(range.end()));

        all.clone()
            .zip(all.skip(1))
            .map(|(lo, hi)| TextRange::new(lo, hi))
            .filter(|it| !it.is_empty())
    }

    pub fn line_at(&self, line_num: usize) -> Option<TextSize> {
        if line_num >= 0 as usize && line_num < self.newlines.len() {
            Some(self.newlines[line_num as usize])
        } else {
            None
        }
    }

    pub fn end_line(&self, line_num: usize) -> TextSize {
        self.newlines[line_num + 1]
    }

    fn utf8_to_utf16_col(&self, line: u32, col: TextSize) -> usize {
        let mut res: usize = col.into();
        if let Some(utf16_chars) = self.utf16_lines.get(&line) {
            for c in utf16_chars {
                if c.end <= col {
                    res -= usize::from(c.len()) - c.len_utf16();
                } else {
                    // From here on, all utf16 characters come *after* the character we are mapping,
                    // so we don't need to take them into account
                    break;
                }
            }
        }
        res
    }

    fn utf16_to_utf8_col(&self, line: u32, mut col: u32) -> TextSize {
        if let Some(utf16_chars) = self.utf16_lines.get(&line) {
            for c in utf16_chars {
                if col > u32::from(c.start) {
                    col += u32::from(c.len()) - c.len_utf16() as u32;
                } else {
                    // From here on, all utf16 characters come *after* the character we are mapping,
                    // so we don't need to take them into account
                    break;
                }
            }
        }

        col.into()
    }
}

pub fn partition_point<T, P>(vec: &[T], mut pred: P) -> usize
where
    P: FnMut(&T) -> bool,
{
    vec.binary_search_by(|x| {
        if pred(x) {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    })
    .unwrap_or_else(|i| i)
}
