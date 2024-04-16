use std::borrow::Cow;
use std::{path::Path, time::Duration};

use similar::{Algorithm, ChangeTag, TextDiff};

use crate::content::yaml;
use crate::snapshot::{MetaData, Snapshot};
use crate::utils::{format_rust_expression, style, term_width};

/// Snapshot printer utility.
pub struct SnapshotPrinter<'a> {
    workspace_root: &'a Path,
    old_snapshot: Option<&'a Snapshot>,
    new_snapshot: &'a Snapshot,
    old_snapshot_hint: &'a str,
    new_snapshot_hint: &'a str,
    show_info: bool,
    show_diff: bool,
    title: Option<&'a str>,
    line: Option<u32>,
    snapshot_file: Option<&'a Path>,
}

impl<'a> SnapshotPrinter<'a> {
    pub fn new(
        workspace_root: &'a Path,
        old_snapshot: Option<&'a Snapshot>,
        new_snapshot: &'a Snapshot,
    ) -> SnapshotPrinter<'a> {
        SnapshotPrinter {
            workspace_root,
            old_snapshot,
            new_snapshot,
            old_snapshot_hint: "old snapshot",
            new_snapshot_hint: "new results",
            show_info: false,
            show_diff: false,
            title: None,
            line: None,
            snapshot_file: None,
        }
    }

    pub fn set_snapshot_hints(&mut self, old: &'a str, new: &'a str) {
        self.old_snapshot_hint = old;
        self.new_snapshot_hint = new;
    }

    pub fn set_show_info(&mut self, yes: bool) {
        self.show_info = yes;
    }

    pub fn set_show_diff(&mut self, yes: bool) {
        self.show_diff = yes;
    }

    pub fn set_title(&mut self, title: Option<&'a str>) {
        self.title = title;
    }

    pub fn set_line(&mut self, line: Option<u32>) {
        self.line = line;
    }

    pub fn set_snapshot_file(&mut self, file: Option<&'a Path>) {
        self.snapshot_file = file;
    }

    pub fn print(&self) {
        if let Some(title) = self.title {
            let width = term_width();
            println!(
                "{title:━^width$}",
                title = style(format!(" {} ", title)).bold(),
                width = width
            );
        }
        self.print_snapshot_diff();
    }

    fn print_snapshot_diff(&self) {
        self.print_snapshot_summary();
        if self.show_diff {
            self.print_changeset();
        } else {
            self.print_snapshot();
        }
    }

    fn print_snapshot_summary(&self) {
        print_snapshot_summary(
            self.workspace_root,
            self.new_snapshot,
            self.snapshot_file,
            self.line,
        );
    }

    fn print_info(&self) {
        print_info(self.new_snapshot.metadata());
    }

    fn print_snapshot(&self) {
        print_line(term_width());

        let new_contents = self.new_snapshot.contents();

        let width = term_width();
        if self.show_info {
            self.print_info();
        }
        println!("Snapshot Contents:");

        match new_contents {
            crate::snapshot::SnapshotContents::Str(new_contents) => {
                println!("──────┬{:─^1$}", "", width.saturating_sub(7));
                for (idx, line) in new_contents.lines().enumerate() {
                    println!("{:>5} │ {}", style(idx + 1).cyan().dim().bold(), line);
                }
                println!("──────┴{:─^1$}", "", width.saturating_sub(7));
            }
            crate::snapshot::SnapshotContents::Image(image) => {
                viuer::print(
                    &image,
                    &viuer::Config {
                        width: Some((width as u32 / 2 - 1).min(64)),
                        absolute_offset: false,
                        ..Default::default()
                    },
                )
                .unwrap();
            }
        }
    }

    fn print_changeset(&self) {
        let width = term_width();
        print_line(width);

        if self.show_info {
            self.print_info();
        }

        match self.new_snapshot.contents() {
            crate::snapshot::SnapshotContents::Str(new) => {
                let old = self
                    .old_snapshot
                    .as_ref()
                    .map_or("", |x| x.contents_str().unwrap());
                let newlines_matter = newlines_matter(old, new);
                let diff = TextDiff::configure()
                    .algorithm(Algorithm::Patience)
                    .timeout(Duration::from_millis(500))
                    .diff_lines(old, new);
                if !old.is_empty() {
                    println!(
                        "{}",
                        style(format_args!("-{}", self.old_snapshot_hint)).red()
                    );
                }
                println!(
                    "{}",
                    style(format_args!("+{}", self.new_snapshot_hint)).green()
                );

                println!("────────────┬{:─^1$}", "", width.saturating_sub(13));
                let mut has_changes = false;
                for (idx, group) in diff.grouped_ops(4).iter().enumerate() {
                    if idx > 0 {
                        println!("┈┈┈┈┈┈┈┈┈┈┈┈┼{:┈^1$}", "", width.saturating_sub(13));
                    }
                    for op in group {
                        for change in diff.iter_inline_changes(op) {
                            match change.tag() {
                                ChangeTag::Insert => {
                                    has_changes = true;
                                    print!(
                                        "{:>5} {:>5} │{}",
                                        "",
                                        style(change.new_index().unwrap()).cyan().dim().bold(),
                                        style("+").green(),
                                    );
                                    for &(emphasized, change) in change.values() {
                                        let change = render_invisible(change, newlines_matter);
                                        if emphasized {
                                            print!("{}", style(change).green().underlined());
                                        } else {
                                            print!("{}", style(change).green());
                                        }
                                    }
                                }
                                ChangeTag::Delete => {
                                    has_changes = true;
                                    print!(
                                        "{:>5} {:>5} │{}",
                                        style(change.old_index().unwrap()).cyan().dim(),
                                        "",
                                        style("-").red(),
                                    );
                                    for &(emphasized, change) in change.values() {
                                        let change = render_invisible(change, newlines_matter);
                                        if emphasized {
                                            print!("{}", style(change).red().underlined());
                                        } else {
                                            print!("{}", style(change).red());
                                        }
                                    }
                                }
                                ChangeTag::Equal => {
                                    print!(
                                        "{:>5} {:>5} │ ",
                                        style(change.old_index().unwrap()).cyan().dim(),
                                        style(change.new_index().unwrap()).cyan().dim().bold(),
                                    );
                                    for &(_, change) in change.values() {
                                        let change = render_invisible(change, newlines_matter);
                                        print!("{}", style(change).dim());
                                    }
                                }
                            }
                            if change.missing_newline() {
                                println!();
                            }
                        }
                    }
                }

                if !has_changes {
                    println!(
                        "{:>5} {:>5} │{}",
                        "",
                        style("-").dim(),
                        style(" snapshots are matching").cyan(),
                    );
                }

                println!("────────────┴{:─^1$}", "", width.saturating_sub(13));
            }
            crate::snapshot::SnapshotContents::Image(new_image) => {
                let image_width = (width as u16 / 2 - 1).min(64);

                if self.old_snapshot.is_some() {
                    print!(
                        "{}",
                        style(format_args!(
                            "{:<1$}",
                            self.old_snapshot_hint,
                            image_width as usize + 1,
                        ))
                        .red()
                    );
                }
                println!(
                    "{}",
                    style(format_args!("{}", self.new_snapshot_hint)).green()
                );

                let print_new = |x, restore_cursor| {
                    viuer::print(
                        new_image,
                        &viuer::Config {
                            x,
                            width: Some(image_width as u32),
                            height: None,
                            absolute_offset: false,
                            restore_cursor,
                            ..Default::default()
                        },
                    )
                    .unwrap();
                };

                if let Some(crate::snapshot::SnapshotContents::Image(old_image)) =
                    self.old_snapshot.map(|s| s.contents())
                {
                    // These heights are in terms of the width of a terminal cell. As such they are not
                    // directly useful for moving the cursor. Instead we can figure out which of the two
                    // images is higher and draw that one last. The restore_cursor flag in viuer makes
                    // use of escape sequences to save and restore the cursor position. This way the
                    // terminal emulator does the job of knowing how high the image is in terms of the
                    // cell height. Otherwise we'd have to use os specific crates for querying the cell
                    // size.
                    let new_height = new_image.height() * image_width as u32 / new_image.width();
                    let old_height = old_image.height() * image_width as u32 / old_image.width();
                    let new_first = new_height < old_height;

                    if new_first {
                        print_new(image_width + 1, true);
                    }

                    viuer::print(
                        old_image,
                        &viuer::Config {
                            width: Some(image_width as u32),
                            height: None,
                            absolute_offset: false,
                            restore_cursor: !new_first,
                            ..Default::default()
                        },
                    )
                    .unwrap();

                    if !new_first {
                        print_new(image_width + 1, false);
                    }
                } else {
                    print_new(0, false);
                };
            }
        }
    }
}

/// Prints the summary of a snapshot
pub fn print_snapshot_summary(
    workspace_root: &Path,
    snapshot: &Snapshot,
    snapshot_file: Option<&Path>,
    mut line: Option<u32>,
) {
    // default to old assertion line from snapshot.
    if line.is_none() {
        line = snapshot.metadata().assertion_line();
    }

    if let Some(snapshot_file) = snapshot_file {
        let snapshot_file = workspace_root
            .join(snapshot_file)
            .strip_prefix(workspace_root)
            .ok()
            .map(|x| x.to_path_buf())
            .unwrap_or_else(|| snapshot_file.to_path_buf());
        println!(
            "Snapshot file: {}",
            style(snapshot_file.display()).cyan().underlined()
        );
    }
    if let Some(name) = snapshot.snapshot_name() {
        println!("Snapshot: {}", style(name).yellow());
    } else {
        println!("Snapshot: {}", style("<inline>").dim());
    }

    if let Some(ref value) = snapshot.metadata().get_relative_source(workspace_root) {
        println!(
            "Source: {}{}",
            style(value.display()).cyan(),
            if let Some(line) = line {
                format!(":{}", style(line).bold())
            } else {
                "".to_string()
            }
        );
    }

    if let Some(ref value) = snapshot.metadata().input_file() {
        println!("Input file: {}", style(value).cyan());
    }
}

fn print_line(width: usize) {
    println!("{:─^1$}", "", width);
}

fn trailing_newline(s: &str) -> &str {
    if s.ends_with("\r\n") {
        "\r\n"
    } else if s.ends_with('\r') {
        "\r"
    } else if s.ends_with('\n') {
        "\n"
    } else {
        ""
    }
}

fn detect_newlines(s: &str) -> (bool, bool, bool) {
    let mut last_char = None;
    let mut detected_crlf = false;
    let mut detected_cr = false;
    let mut detected_lf = false;

    for c in s.chars() {
        if c == '\n' {
            if last_char.take() == Some('\r') {
                detected_crlf = true;
            } else {
                detected_lf = true;
            }
        }
        if last_char == Some('\r') {
            detected_cr = true;
        }
        last_char = Some(c);
    }
    if last_char == Some('\r') {
        detected_cr = true;
    }

    (detected_cr, detected_crlf, detected_lf)
}

fn newlines_matter(left: &str, right: &str) -> bool {
    if trailing_newline(left) != trailing_newline(right) {
        return true;
    }

    let (cr1, crlf1, lf1) = detect_newlines(left);
    let (cr2, crlf2, lf2) = detect_newlines(right);

    !matches!(
        (cr1 || cr2, crlf1 || crlf2, lf1 || lf2),
        (false, false, false) | (true, false, false) | (false, true, false) | (false, false, true)
    )
}

fn render_invisible(s: &str, newlines_matter: bool) -> Cow<'_, str> {
    if newlines_matter || s.find(&['\x1b', '\x07', '\x08', '\x7f'][..]).is_some() {
        Cow::Owned(
            s.replace('\r', "␍\r")
                .replace('\n', "␊\n")
                .replace("␍\r␊\n", "␍␊\r\n")
                .replace('\x07', "␇")
                .replace('\x08', "␈")
                .replace('\x1b', "␛")
                .replace('\x7f', "␡"),
        )
    } else {
        Cow::Borrowed(s)
    }
}

fn print_info(metadata: &MetaData) {
    let width = term_width();
    if let Some(expr) = metadata.expression() {
        println!("Expression: {}", style(format_rust_expression(expr)));
        print_line(width);
    }
    if let Some(descr) = metadata.description() {
        println!("{}", descr);
        print_line(width);
    }
    if let Some(info) = metadata.private_info() {
        let out = yaml::to_string(info);
        // TODO: does the yaml output always start with '---'?
        println!("{}", out.trim().strip_prefix("---").unwrap().trim_start());
        print_line(width);
    }
}

#[test]
fn test_invisible() {
    assert_eq!(
        render_invisible("\r\n\x1b\r\x07\x08\x7f\n", true),
        "␍␊\r\n␛␍\r␇␈␡␊\n"
    );
}
