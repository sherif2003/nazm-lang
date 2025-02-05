use std::{
    cell::Cell,
    collections::HashMap,
    fmt::{self, Display},
    rc::Rc,
};

use itertools::Itertools;
use owo_colors::{OwoColorize, Style};
use painter::Painter;

use crate::{
    file_info::FileInfo,
    span::{Span, SpanCursor},
};

mod painter;

pub struct CodeWindow<'a> {
    file_path: &'a str,
    file_lines: &'a [String],
    cursor: SpanCursor,
    /// Map lines indecies and main depth line on them
    code_lines: HashMap<usize, CodeLine<'a>>,
}

impl<'a> CodeWindow<'a> {
    pub fn new(file_info: &'a FileInfo, cursor: SpanCursor) -> Self {
        Self {
            file_path: &file_info.path,
            file_lines: &file_info.lines,
            cursor,
            code_lines: HashMap::new(),
        }
    }

    pub fn mark_error(&mut self, span: Span, labels: Vec<String>) -> &mut Self {
        self.mark(span, '^', Style::new().bold().red(), labels);
        self
    }

    pub fn mark_warning(&mut self, span: Span, labels: Vec<String>) -> &mut Self {
        self.mark(span, '^', Style::new().bold().yellow(), labels);
        self
    }

    pub fn mark_help(&mut self, span: Span, labels: Vec<String>) -> &mut Self {
        self.mark(span, '=', Style::new().bold().cyan(), labels);
        self
    }

    pub fn mark_note(&mut self, span: Span, labels: Vec<String>) -> &mut Self {
        self.mark(span, '~', Style::new().bold().green(), labels);
        self
    }

    pub fn mark_secondary(&mut self, span: Span, labels: Vec<String>) -> &mut Self {
        self.mark(span, '-', Style::new().bold().blue(), labels);
        self
    }

    pub fn mark_tertiary(&mut self, span: Span, labels: Vec<String>) -> &mut Self {
        self.mark(span, '*', Style::new().bold().bright_magenta(), labels);
        self
    }

    fn mark(&mut self, span: Span, sign: char, style: Style, labels: Vec<String>) -> &mut Self {
        let start_line = span.start.line;
        let start_col = span.start.col;
        let end_line = span.end.line;
        let end_col = span.end.col;

        if start_line == end_line {
            self.code_lines
                .entry(start_line)
                .or_insert(CodeLine::default())
                .mark_as_one_line(start_col, end_col, sign, style, labels);

            return self;
        }

        let connection_margin = Rc::default(); // It will be updated later

        self.code_lines
            .entry(start_line)
            .or_insert(CodeLine::default())
            .mark_as_multi_line_start(start_col, sign, style, Rc::clone(&connection_margin));

        self.code_lines
            .entry(end_line)
            .or_insert(CodeLine::default())
            .mark_as_multi_line_end(end_col, sign, style, labels, connection_margin);

        // Add the line between them if possible

        if start_line + 2 == end_line {
            self.code_lines
                .entry(start_line + 1)
                .or_insert(CodeLine::default());
        }

        return self;
    }
}

impl<'a> Display for CodeWindow<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut free_connection_margins = vec![];
        let mut connections_painter = Painter::new(
            Marker {
                sign: MarkerSign::Char(' '),
                style: Style::new(),
            }, // Default is space
        );
        let mut big_sheet = vec![];
        let mut lines_indecies = self.code_lines.keys().sorted();

        let mut num_of_displayed_lines = 0;

        let mut max_line_num = 0; // This is needed later and will be updated in the loop to not calculate it by keys.max()

        for line_index in lines_indecies.clone() {
            max_line_num = line_index + 1; // Add one

            let code_line = &self.code_lines[line_index];

            let file_line = &self.file_lines[*line_index];

            big_sheet.push(vec![vec![Marker {
                sign: MarkerSign::CodeLine(&file_line),
                style: Style::new(),
            }]]);

            num_of_displayed_lines += 1;

            // This will always align the connections sheet with other sheets
            connections_painter
                .move_to_zero()
                .move_down_by(num_of_displayed_lines);

            let painter_opt = code_line.draw(
                &mut free_connection_margins,
                &mut connections_painter,
                &file_line,
            );

            match painter_opt {
                Some(painter) => {
                    let small_sheet = painter.get_sheet();
                    num_of_displayed_lines += small_sheet.len();
                    big_sheet.push(small_sheet);
                }
                None => {}
            }
        }

        let connections_sheet = connections_painter.get_sheet();
        let mut connections = connections_sheet.iter();
        let max_margin = free_connection_margins.len() * 2;

        let max_line_num_indent = max_line_num.to_string().len();

        let line_nums_style = Style::new().blue().bold();

        let _ = writeln!(
            f,
            "{}{} {}:{}:{}",
            " ".repeat(max_line_num_indent).style(line_nums_style),
            "-->".style(line_nums_style),
            self.file_path,
            self.cursor.line + 1,
            self.cursor.col + 1
        );

        let _ = write!(
            f,
            "{} {}",
            " ".repeat(max_line_num_indent).style(line_nums_style),
            '|'.style(line_nums_style)
        );

        // This is needed to add `...` between non-continues lines
        // i.e., display line 5 then display `...` then display line 10
        // So we need it to compare current line with previous one

        let mut prev_line_num = 0; // Start with zero (This will store the line num and not the its index)

        for line_of_markers in big_sheet.iter().flatten() {
            let _ = writeln!(f);

            let next_connect_line_opt = connections.next();

            if line_of_markers.len() == 1
                && matches!(line_of_markers[0].sign, MarkerSign::CodeLine(_))
            {
                let current_line_num = lines_indecies.next().unwrap() + 1;
                if prev_line_num > 0 && prev_line_num + 1 < current_line_num {
                    if prev_line_num + 2 == current_line_num {
                        let line_num_str = (prev_line_num + 1).to_string();
                        let _ = write!(
                            f,
                            "{}{} {} ",
                            line_num_str.style(line_nums_style),
                            " ".repeat(max_line_num_indent - line_num_str.len()),
                            '|'.style(line_nums_style),
                        );
                    } else {
                        let _ = write!(
                            f,
                            "{}{}",
                            "...".style(line_nums_style),
                            " ".repeat(max_line_num_indent).style(line_nums_style),
                        );
                    }
                    // Copying the next connections
                    if let Some(connection_line) = next_connect_line_opt {
                        if let Some(len) = max_margin.checked_sub(connection_line.len()) {
                            let _ = write!(f, "{}", " ".repeat(len));
                        }
                        for c in connection_line.iter().rev() {
                            let _ = write!(
                                f,
                                "{}",
                                if let MarkerSign::Char('|') = c.sign {
                                    c.clone_with_char('|')
                                } else {
                                    c.clone_with_char(' ')
                                }
                            );
                        }
                    }
                    if prev_line_num + 2 == current_line_num {
                        let _ = write!(f, "{}", self.file_lines[prev_line_num],);
                    }
                    let _ = writeln!(f);
                }
                prev_line_num = current_line_num;
                let line_num_str = prev_line_num.to_string();
                let _ = write!(
                    f,
                    "{}{} {} ",
                    line_num_str.style(line_nums_style),
                    " ".repeat(max_line_num_indent - line_num_str.len()),
                    '|'.style(line_nums_style)
                );
            } else {
                let _ = write!(
                    f,
                    "{} {} ",
                    " ".repeat(max_line_num_indent).style(line_nums_style),
                    '|'.style(line_nums_style)
                );
            }

            if let (Some(connection_line), true) = (next_connect_line_opt, max_margin > 0) {
                let _ = write!(f, "{}", " ".repeat(max_margin - connection_line.len()));
                for c in connection_line.iter().rev() {
                    let _ = write!(f, "{c}");
                }
            } else {
                let _ = write!(f, "{}", " ".repeat(max_margin));
            }
            for marker in line_of_markers {
                let _ = write!(f, "{marker}");
            }
        }

        Ok(())
    }
}

#[derive(Default)]
struct CodeLine<'a> {
    /// Map column indecies to markers on them
    markers: HashMap<usize, (Marker<'a>, MarkerType)>,
}

impl<'a> CodeLine<'a> {
    fn mark_as_one_line(
        &mut self,
        start_col: usize,
        end_col: usize,
        sign: char,
        style: Style,
        labels: Vec<String>,
    ) {
        let marker = Marker {
            sign: MarkerSign::Char(sign),
            style: style,
        };
        self.markers.insert(
            start_col,
            (
                marker,
                MarkerType::OneLineStart {
                    end_col: end_col,
                    labels: labels,
                },
            ),
        );
    }

    fn mark_as_multi_line_start(
        &mut self,
        col: usize,
        sign: char,
        style: Style,
        connection_margin: Rc<Cell<(usize, usize)>>,
    ) {
        let marker = Marker {
            sign: MarkerSign::Char(sign),
            style: style,
        };
        self.markers.insert(
            col,
            (
                marker,
                MarkerType::StartOfMultiLine {
                    connection_margin: connection_margin,
                },
            ),
        );
    }

    fn mark_as_multi_line_end(
        &mut self,
        col: usize,
        sign: char,
        style: Style,
        labels: Vec<String>,
        connection_margin: Rc<Cell<(usize, usize)>>,
    ) {
        let marker = Marker {
            sign: MarkerSign::Char(sign),
            style: style,
        };
        self.markers.insert(
            col,
            (
                marker,
                MarkerType::EndOfMultiLine {
                    connection_margin: connection_margin,
                    labels: labels,
                },
            ),
        );
    }

    fn draw(
        &'a self,
        free_connection_margins: &mut Vec<bool>,
        connections_painter: &mut Painter<Marker<'a>>,
        file_line: &'a str,
    ) -> Option<Painter<Marker<'a>>> {
        let mut painter = Painter::new(
            Marker {
                sign: MarkerSign::Char(' '),
                style: Style::new(),
            }, // Default is space
        );

        let painter_local_zero = painter.current_brush_pos();

        let connections_painter_local_zero = connections_painter.current_brush_pos();

        // This is a special case when the multiline marker starts after spaces
        // It will make the marker starts with `/` from connections sheet not from the normal sheet
        /*
         * /      Code
         * | Code Code
         * |_________^
         */

        let min_col_opt = self.markers.keys().min();

        if min_col_opt.is_none() {
            // No markers
            return None; // No markers to draw in the main sheet
        }

        let min_col = min_col_opt.unwrap();

        let mut min_col_is_after_spaces = false;

        if let (MarkerType::StartOfMultiLine { connection_margin }, true) = (
            &self.markers[min_col].1,
            file_line.starts_with(&" ".repeat(*min_col)),
        ) {
            min_col_is_after_spaces = true;

            connections_painter.move_up(); // To align with the code line

            let brush_pos = connections_painter.current_brush_pos();

            let mut is_free_margin_found = false;
            let mut found_margin = 0;
            for (margin, is_free) in free_connection_margins.iter().enumerate() {
                if *is_free {
                    free_connection_margins[margin] = false;
                    is_free_margin_found = true;
                    found_margin = margin;
                    break;
                }
            }

            if !is_free_margin_found {
                found_margin = free_connection_margins.len();
                free_connection_margins.push(false);
            }

            (*connection_margin).set((brush_pos.0, found_margin));

            connections_painter
                .move_right_by(2 * found_margin + 1)
                .paint(self.markers[min_col].0.clone_with_char('/'));

            if self.markers.len() == 1 {
                connections_painter.move_down(); // To reset the line 208
                return None; // No markers to draw in the main sheet
            }
        }

        // The number of bars (`|`) between the code and the next label (of one-line markers and multiline end markers)
        let mut next_labels_margin = 0;

        // The number of bars (`|`) between the code and the repeated underscores (`_`) of multiline marker
        let mut next_multline_margin = 0;

        for (col_enum, col) in self.markers.keys().sorted().rev().enumerate() {
            painter.move_to(painter_local_zero);
            connections_painter.move_to(connections_painter_local_zero);

            let (marker, marker_typ) = &self.markers[col];

            match marker_typ {
                MarkerType::OneLineStart { end_col, labels } => {
                    painter.move_right_by(*end_col);
                    let brush_pos = painter.current_brush_pos();
                    for _ in 0..*end_col - col {
                        painter.move_left().paint(marker.clone());
                    }
                    if !labels.is_empty() {
                        // Check if this label margin will be less than the next multiline marker margin
                        // This will prevent labels to be above the next multiline margin
                        // But they may have the same margins
                        /*
                         * Code Code Code
                         *      ^    ^^^^
                         * _____|    |
                         *           Label
                         */
                        if next_labels_margin < next_multline_margin {
                            // The margin of this label should equal the next multiline margin
                            next_labels_margin = next_multline_margin;
                        }
                        if next_labels_margin == 0 {
                            painter.move_to(brush_pos);
                        } else {
                            for _depth in 0..next_labels_margin {
                                painter.move_down().paint(marker.clone_with_char('|'));
                            }
                            painter.move_down();
                        }

                        // Increase the labels margin by number of labels and if it's greater than one subtract one
                        /*
                        *
                        * احجز متغير س = 555؛
                           ^^^^_^^^^^___~ ---من الممكن عدم جعل القيمة متغيرة
                           |    |       |    من الممكن عدم جعل القيمة متغيرة  (remove extra one margin if they're more than one and we are on the first label in reverse)
                           |    |       من الممكن عدم جعل القيمة متغيرة
                           |    من الممكن عدم جعل القيمة متغيرة
                           من الممكن عدم جعل القيمة متغيرة

                        */
                        next_labels_margin +=
                            labels.len() - (labels.len() > 1 && next_labels_margin == 0) as usize;

                        // Add a space if it is the first marker from the end
                        if col_enum == 0 {
                            painter.move_right();
                        }

                        for (i, label) in labels.iter().enumerate() {
                            if i != 0 {
                                painter.move_down();
                            }

                            painter.paint(marker.clone_with_str(label));
                        }

                        // Remove the added space
                        if col_enum == 0 {
                            painter.move_left();
                        }
                    } else if next_labels_margin == 0 {
                        // Increase the labels margin if we are on the first marker from reverse and there is no labels found
                        next_labels_margin += 1;
                    }
                }
                MarkerType::EndOfMultiLine {
                    connection_margin,
                    labels,
                } => {
                    painter.move_right_by(*col);

                    let brush_pos = painter.current_brush_pos();

                    painter.move_left().paint(marker.clone());

                    if !labels.is_empty() {
                        // Note it increase the labels depth if they're equal
                        // as labels of multiline end markers have at least one depth greater than it's depth
                        if next_labels_margin <= next_multline_margin && next_multline_margin != 0 {
                            // This happen if it is not the first multiline
                            next_labels_margin = next_multline_margin + 1; // Labels should be below
                        }
                        if next_labels_margin == 0 {
                            painter.move_to(brush_pos);
                        } else {
                            for _depth in 0..next_labels_margin {
                                painter.move_down().paint(marker.clone_with_char('|'));
                            }
                            painter.move_down();
                        }

                        // Increase the labels margin by number of labels and if it's greater than one subtract one
                        next_labels_margin +=
                            labels.len() - (labels.len() > 1 && next_labels_margin == 0) as usize;

                        // Add a space if it is the first marker from the end
                        if col_enum == 0 {
                            painter.move_right();
                        }

                        for (i, label) in labels.iter().enumerate() {
                            if i != 0 {
                                painter.move_down();
                            }
                            painter.paint(marker.clone_with_str(label));
                        }

                        // Remove the added space
                        if col_enum == 0 {
                            painter.move_left();
                        }
                    }

                    painter
                        .move_to(brush_pos)
                        .move_down_by(next_multline_margin)
                        .move_left();

                    // If the end col is 1 it will override the marker putted there
                    // As calling move_left will not do any thing but it will paint '_'
                    if *col != 1 {
                        for _ in 0..*col {
                            painter.move_left().paint(marker.clone_with_char('_'));
                        }
                    }

                    connections_painter.move_down_by(next_multline_margin);

                    let margin = connection_margin.get().1;

                    free_connection_margins[margin] = true; // free this margin

                    let brush_pos = connections_painter.current_brush_pos();

                    for _ in 0..=margin * 2 {
                        connections_painter
                            .paint(marker.clone_with_char('_'))
                            .move_right();
                    }

                    for _ in connection_margin.get().0..brush_pos.0 {
                        connections_painter
                            .paint(marker.clone_with_char('|'))
                            .move_up();
                    }

                    next_multline_margin += 1;
                }
                MarkerType::StartOfMultiLine { connection_margin } => {
                    // This is a special case when the multiline marker starts after spaces
                    // It will make the marker starts with `/` from connections sheet not from the normal sheet
                    /*
                     * /      Code
                     * | Code Code
                     * |_________^
                     */
                    if col == min_col && min_col_is_after_spaces {
                        break; // No more iterations as it is the last column
                    }

                    painter.move_right_by(*col).paint(marker.clone());

                    for _depth in 0..next_multline_margin {
                        painter.move_down().paint(marker.clone_with_char('|'));
                    }

                    for _ in 0..*col {
                        painter.move_left().paint(marker.clone_with_char('_'));
                    }

                    connections_painter.move_down_by(next_multline_margin);

                    let brush_pos = connections_painter.current_brush_pos();

                    let mut is_free_margin_found = false;
                    let mut found_margin = 0;
                    for (margin, is_free) in free_connection_margins.iter().enumerate() {
                        if *is_free {
                            free_connection_margins[margin] = false;
                            is_free_margin_found = true;
                            found_margin = margin;
                            break;
                        }
                    }

                    if !is_free_margin_found {
                        found_margin = free_connection_margins.len();
                        free_connection_margins.push(false);
                    }

                    (*connection_margin).set((brush_pos.0, found_margin));

                    for _ in 0..=found_margin * 2 {
                        connections_painter
                            .paint(marker.clone_with_char('_'))
                            .move_right();
                    }

                    next_multline_margin += 1;
                }
            }
        }

        return Some(painter);
    }
}

#[derive(Clone)]
struct Marker<'a> {
    sign: MarkerSign<'a>,
    style: Style,
}

impl<'a> Marker<'a> {
    #[inline]
    fn new_char(ch: char, style: Style) -> Self {
        Self {
            sign: MarkerSign::Char(ch),
            style: style,
        }
    }

    #[inline]
    fn new_str(s: &'a str, style: Style) -> Self {
        Self {
            sign: MarkerSign::Str(s),
            style: style,
        }
    }

    #[inline]
    fn clone_with_char(&self, ch: char) -> Self {
        Self::new_char(ch, self.style)
    }

    #[inline]
    fn clone_with_str(&self, s: &'a str) -> Self {
        Self::new_str(s, self.style)
    }
}

impl<'a> Display for Marker<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.sign {
            MarkerSign::Char(c) => write!(f, "{}", c.style(self.style)),
            MarkerSign::Str(s) => write!(f, "{}", s.style(self.style)),
            MarkerSign::CodeLine(l) => write!(f, "{}", l.style(self.style)),
        }
    }
}

#[derive(Clone)]
enum MarkerSign<'a> {
    Char(char),
    Str(&'a str),
    CodeLine(&'a str),
}

#[derive(Clone)]
enum MarkerType {
    OneLineStart {
        end_col: usize,
        labels: Vec<String>,
    },
    StartOfMultiLine {
        /// This margin and the end marker counter-part should agree on the same margin to connect between them correctly
        ///
        /// The first is the current row index of the brush of the connection painter
        ///
        /// The second is the margin index found in the free connection margins array
        ///
        /// The end counter-part is responsible to draw the connections from it to the position of the start counter-part
        connection_margin: Rc<Cell<(usize, usize)>>,
    },
    EndOfMultiLine {
        /// This margin and the start marker counter-part should agree on the same margin to connect between them correctly
        ///
        /// The first is the row index of the brush of the connection painter at the start counter-part
        ///
        /// The second is the margin index found in the free connection margins array
        ///
        /// The end counter-part is responsible to draw the connections from it to the position of the start counter-part
        connection_margin: Rc<Cell<(usize, usize)>>,
        labels: Vec<String>,
    },
}

#[cfg(test)]
mod tests {

    use std::{
        collections::HashMap,
        fmt::Debug,
        io::{self, Write},
        process::Command,
    };

    use owo_colors::{Style, XtermColors};

    use crate::span::{Span, SpanCursor};

    use super::CodeWindow;

    fn rtl() {
        // RTL printing
        let output = Command::new("printf").arg(r#""\e[2 k""#).output().unwrap();
        io::stdout()
            .write_all(&output.stdout[1..output.stdout.len() - 1])
            .unwrap();
    }

    macro_rules! get_code_reporter {
        () => {{
            rtl();
            CodeWindow {
                file_path: "اختبار.نظم",
                file_lines: &[
                    "حجز متغير أ = 555؛".to_string(),
                    "حجز متغير ب = 555؛".to_string(),
                    "حجز متغير ج = 555؛".to_string(),
                    "حجز متغير د = 555؛".to_string(),
                    "حجز متغير ه = 555؛".to_string(),
                    "حجز متغير و = 555؛".to_string(),
                    "حجز متغير ز = 555؛".to_string(),
                    "حجز متغير ح = 555؛".to_string(),
                    "حجز متغير ط = 555؛".to_string(),
                    "حجز متغير ي = 555؛".to_string(),
                    "حجز متغير ك = 555؛".to_string(),
                    "حجز متغير ل = 555؛".to_string(),
                    "حجز متغير م = 555؛".to_string(),
                    "حجز متغير ن = 555؛".to_string(),
                    "حجز متغير س = 555؛".to_string(),
                    "حجز متغير ع = 555؛".to_string(),
                    "حجز متغير ف = 555؛".to_string(),
                    "حجز متغير ص = 555؛".to_string(),
                    "حجز متغير ق = 555؛".to_string(),
                    "حجز متغير ر = 555؛".to_string(),
                    "حجز متغير ش = 555؛".to_string(),
                    "حجز متغير ت = 555؛".to_string(),
                    "حجز متغير ث = 555؛".to_string(),
                    "حجز متغير خ = 555؛".to_string(),
                    "حجز متغير ذ = 555؛".to_string(),
                    "حجز متغير ض = 555؛".to_string(),
                    "حجز متغير ظ = 555؛".to_string(),
                    "حجز متغير غ = 555؛".to_string(),
                ],
                cursor: SpanCursor { line: 0, col: 0 },
                code_lines: HashMap::new(),
            }
        }};
    }

    #[test]
    fn test_one_line() {
        let mut reporter = get_code_reporter!();

        reporter.mark(
            Span::new((0, 0), (0, 3)),
            '?',
            Style::new().blue().cyan(),
            vec!["القيمة ليست متغيرة".to_string()],
        );

        println!("{}", reporter)
    }

    #[test]
    fn test_multi_line() {
        let mut reporter = get_code_reporter!();

        reporter.mark(
            Span::new((0, 4), (1, 5)),
            '^',
            Style::new().red().bold(),
            vec!["القيمة ليست متغيرة".to_string()],
        );

        println!("{}", reporter)
    }

    #[test]
    fn test_multi_line_ends_at_first_column() {
        let mut reporter = get_code_reporter!();

        reporter.mark(
            Span::new((0, 4), (1, 1)),
            '^',
            Style::new().italic().red().bold(),
            vec!["القيمة ليست متغيرة".to_string()],
        );

        println!("{}", reporter)
    }

    #[test]
    fn test_reporting_complex() {
        let mut reporter = get_code_reporter!();

        reporter
            .mark(
                Span::new((0, 0), (0, 3)),
                '?',
                Style::new().cyan(),
                vec!["القيمة ليست متغيرة".to_string()],
            )
            .mark(
                Span::new((0, 14), (0, 17)),
                '~',
                Style::new().green(),
                vec![
                    "القيمة ليست متغيرة".to_string(),
                    "القيمة ليست متغيرة".to_string(),
                    "القيمة ليست متغيرة".to_string(),
                ],
            )
            .mark(
                Span::new((0, 4), (0, 9)),
                '-',
                Style::new().blue().bold(),
                vec!["القيمة ليست متغيرة".to_string()],
            )
            .mark(
                Span::new((2, 4), (2, 9)),
                '^',
                Style::new().yellow().bold(),
                vec![
                    "القيمة ليست متغيرة".to_string(),
                    "القيمة ليست متغيرة".to_string(),
                ],
            )
            .mark(
                Span::new((1, 4), (2, 3)),
                '^',
                Style::new().red().bold(),
                vec![
                    "علامة طويلة".to_string(),
                    "علامة طويلة".to_string(),
                    "علامة طويلة".to_string(),
                    "علامة طويلة".to_string(),
                    "ما قولتلك يا بني علامة طويلة".to_string(),
                ],
            )
            .mark(
                Span::new((1, 15), (2, 18)),
                '^',
                Style::new().color(XtermColors::FlushOrange).bold(),
                vec!["علامة طويلة".to_string()],
            )
            .mark(
                Span::new((0, 12), (2, 13)),
                '^',
                Style::new().color(XtermColors::PinkFlamingo).bold(),
                vec!["علامة طويلة".to_string()],
            )
            .mark(
                Span::new((1, 0), (2, 1)),
                '^',
                Style::new().color(XtermColors::Brown).bold(),
                vec!["علامة طويلة".to_string()],
            )
            .mark(
                Span::new((0, 10), (1, 3)),
                '^',
                Style::new().magenta().bold(),
                vec![
                    "علامة طويلة".to_string(),
                    "علامة طويلة".to_string(),
                    "علامة طويلة".to_string(),
                ],
            )
            .mark(
                Span::new((1, 8), (1, 9)),
                '^',
                Style::new().color(XtermColors::Bermuda).bold(),
                vec![
                    "علامة طويلة".to_string(),
                    "علامة طويلة".to_string(),
                    "علامة طويلة".to_string(),
                ],
            )
            .mark(
                Span::new((3, 4), (6, 9)),
                '^',
                Style::new().color(XtermColors::GreenYellow).bold(),
                vec!["علامة طويلة".to_string()],
            )
            .mark(
                Span::new((4, 10), (5, 5)),
                '^',
                Style::new().color(XtermColors::BayLeaf).bold(),
                vec!["علامة طويلة".to_string()],
            )
            .mark(
                Span::new((7, 15), (7, 19)),
                '^',
                Style::new().color(XtermColors::Dandelion).bold(),
                vec!["علامة طويلة".to_string()],
            )
            .mark(
                Span::new((7, 0), (9, 3)),
                '^',
                Style::new().color(XtermColors::Caramel).bold(),
                vec!["علامة طويلة".to_string()],
            )
            .mark(
                Span::new((7, 4), (9, 9)),
                '^',
                Style::new().color(XtermColors::CanCanPink).bold(),
                vec!["علامة طويلة".to_string()],
            )
            .mark(
                Span::new((7, 9), (9, 15)),
                '^',
                Style::new().color(XtermColors::DarkRose).bold(),
                vec!["علامة طويلة".to_string()],
            )
            .mark(
                Span::new((7, 12), (9, 18)),
                '^',
                Style::new().color(XtermColors::Dandelion).bold(),
                vec!["علامة طويلة".to_string()],
            )
            .mark(
                Span::new((11, 4), (12, 5)),
                '^',
                Style::new().color(XtermColors::Dandelion).bold(),
                vec!["علامة طويلة".to_string()],
            )
            .mark(
                Span::new((14, 4), (20, 5)),
                '^',
                Style::new().color(XtermColors::BittersweetOrange).bold(),
                vec!["علامة طويلة".to_string()],
            )
            .mark(
                Span::new((17, 4), (17, 9)),
                '^',
                Style::new().color(XtermColors::CaribbeanGreen).bold(),
                vec!["علامة طويلة".to_string()],
            )
            .mark(
                Span::new((0, 0), (17, 1)),
                '^',
                Style::new().color(XtermColors::BlueStone).bold(),
                vec!["علامة طويلة".to_string()],
            );

        println!("{}", reporter);
    }
}
