/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

use anyhow::Result;
#[cfg(not(test))]
pub use progress::*;
#[cfg(test)]
pub use test_progress::*;

use crate::load_rebar::LoadResult;

pub fn compile_deps(loaded: &LoadResult) -> Result<()> {
    let pb = spinner("Compiling dependencies", "Compiled dependencies");
    loaded.project.compile_deps()?;
    pb.finish();
    Ok(())
}

#[allow(dead_code)]
mod progress {
    use std::borrow::Cow;
    use std::time::Duration;

    use indicatif::ProgressBar;
    use indicatif::ProgressFinish;
    use indicatif::ProgressStyle;

    pub fn progress(len: u64, message: &'static str, done_message: &'static str) -> ProgressBar {
        let pb = ProgressBar::new(len);
        pb.set_style(
            ProgressStyle::default_bar()
                .template("  {msg:25} {bar} {pos}/{len}")
                .expect("BUG: invalid template"),
        );
        pb.set_message(message);
        pb.with_finish(ProgressFinish::WithMessage(Cow::Borrowed(done_message)))
    }

    pub fn spinner(message: &'static str, done_message: &'static str) -> ProgressBar {
        let pb = ProgressBar::new_spinner();
        pb.enable_steady_tick(Duration::from_millis(120));
        pb.set_message(message);
        pb.with_finish(ProgressFinish::WithMessage(Cow::Borrowed(done_message)))
    }
}

#[allow(dead_code)]
mod test_progress {
    use indicatif::ProgressBar;

    pub fn progress(_len: u64, _message: &str, _done_message: &str) -> ProgressBar {
        ProgressBar::hidden()
    }

    pub fn spinner(_message: &str, _done_message: &str) -> ProgressBar {
        ProgressBar::hidden()
    }
}
