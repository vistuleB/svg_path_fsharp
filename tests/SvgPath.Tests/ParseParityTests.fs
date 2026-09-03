module SvgPath.Tests.ParseParityTests

open SvgPath
open Xunit

let private canonical source = source |> Parse.path |> Result.defaultWith (failwithf "%A") |> Serialize.path
let private succeeds source = match Parse.path source with | Ok _ -> () | Error e -> failwithf "%A" e
let private fails source = match Parse.path source with | Error _ -> () | Ok p -> failwithf "unexpectedly parsed: %s" (Serialize.path p)

[<Fact>]
let ``generated valid coordinate separator cases`` () =
    let separators = [ " "; "  "; "\t"; "\n"; "\r"; "\u000c"; ","; " , " ]
    let numbers = [ "0"; "-1"; "+2"; ".5"; "-.25"; "1e2"; "2E-1" ]
    for separator in separators do
        for number in numbers do
            succeeds ("M" + number + separator + number)

[<Fact>]
let ``generated valid signed number boundaries`` () =
    [ "M0-1"; "M0+1"; "M.5-.25"; "M1e2-2e1"; "M1e-2+3e-4"; "M0 0L1-2-3+4" ]
    |> List.iter succeeds

[<Fact>]
let ``generated valid compact arc flag cases`` () =
    for largeArc in [ 0; 1 ] do
        for sweep in [ 0; 1 ] do
            for endpoint in [ "10 20"; "-10-20"; "+10+20" ] do
                succeeds (sprintf "M0 0A5 8 30 %d%d%s" largeArc sweep endpoint)

[<Fact>]
let ``generated invalid comma placement cases`` () =
    [ ",M0 0"; "M,0 0"; "M0,,0"; "M0, ,0"; "M0 0,"; "M0 0,L1 1"; "M0 0 L,1 1"; "M0 0 Z," ]
    |> List.iter fails

[<Fact>]
let ``generated invalid arc flag cases`` () =
    for flag in [ -3; -2; -1; 2; 3; 4; 5; 6; 7; 8; 9 ] do
        fails (sprintf "M0 0A5 5 0 %d 0 10 10" flag)
        fails (sprintf "M0 0A5 5 0 0 %d 10 10" flag)

[<Fact>]
let ``empty_string_parses_as_empty_path_test`` () =
    Assert.Equal("", canonical "")

[<Fact>]
let ``absolute_lines_parse_test`` () =
    Assert.Equal("M 0 0 H 10 V 20 H 0", canonical "M 0 0 L 10 0 V 20 H 0")

[<Fact>]
let ``relative_lines_parse_to_absolute_segments_test`` () =
    Assert.Equal("M 10 10 H 15 V 30 H 10", canonical "m 10 10 l 5 0 v 20 h -5")

[<Fact>]
let ``relative_move_after_close_uses_closed_subpath_start_test`` () =
    Assert.Equal("M 0 0 H 10 Z M 5 5 H 10", canonical "M 0 0 L 10 0 z m 5 5 l 5 0")

[<Fact>]
let ``repeated_line_horizontal_and_vertical_values_parse_test`` () =
    Assert.Equal("M 0 0 H 10 V 10 H 5 H 0 V 5 V 0", canonical "M 0 0 L 10 0 10 10 H 5 0 V 5 0")

[<Fact>]
let ``repeated_absolute_line_coordinate_pairs_parse_test`` () =
    Assert.Equal("M 0 0 L 5 10 L 3 2 L 4 5 L 2 2 V 3", canonical "M 0 0 L 5 10 3 2 4 5 2 2 V 3")

[<Fact>]
let ``repeated_relative_line_coordinate_pairs_parse_test`` () =
    Assert.Equal("M 0 0 L 5 10 L 8 12 L 12 17 L 14 19 V 22", canonical "m 0 0 l 5 10 3 2 4 5 2 2 v 3")

[<Fact>]
let ``repeated_line_pairs_can_switch_to_horizontal_command_test`` () =
    Assert.Equal("M 1 1 L 5 10 L 3 2 L 4 5 H 9", canonical "M 1 1 L 5 10 3 2 4 5 H 9")

[<Fact>]
let ``absolute_quadratic_beziers_parse_test`` () =
    Assert.Equal("M 0 0 Q 10 20 30 40", canonical "M 0 0 Q 10 20 30 40")

[<Fact>]
let ``relative_quadratic_beziers_parse_test`` () =
    Assert.Equal("M 10 10 Q 15 20 30 40", canonical "M 10 10 q 5 10 20 30")

[<Fact>]
let ``repeated_quadratic_beziers_parse_test`` () =
    Assert.Equal("M 0 0 Q 10 20 30 40 T 70 80", canonical "M 0 0 Q 10 20 30 40 50 60 70 80")

[<Fact>]
let ``absolute_smooth_quadratic_beziers_parse_test`` () =
    Assert.Equal("M 0 0 Q 10 20 30 40 T 50 60", canonical "M 0 0 Q 10 20 30 40 T 50 60")

[<Fact>]
let ``relative_smooth_quadratic_beziers_parse_test`` () =
    Assert.Equal("M 10 10 Q 15 20 30 40 T 50 60", canonical "M 10 10 q 5 10 20 30 t 20 20")

[<Fact>]
let ``repeated_smooth_quadratic_beziers_parse_test`` () =
    Assert.Equal("M 0 0 Q 10 20 30 40 T 50 60 T 70 80", canonical "M 0 0 Q 10 20 30 40 T 50 60 70 80")

[<Fact>]
let ``smooth_quadratic_without_previous_quadratic_uses_current_point_test`` () =
    Assert.Equal("M 0 0 L 5 5 T 10 10", canonical "M 0 0 L 5 5 T 10 10")

[<Fact>]
let ``absolute_cubic_beziers_parse_test`` () =
    Assert.Equal("M 0 0 C 10 20 30 40 50 60", canonical "M 0 0 C 10 20 30 40 50 60")

[<Fact>]
let ``relative_cubic_beziers_parse_test`` () =
    Assert.Equal("M 10 10 C 11 12 13 14 15 16", canonical "M 10 10 c 1 2 3 4 5 6")

[<Fact>]
let ``repeated_cubic_beziers_parse_test`` () =
    Assert.Equal("M 0 0 C 1 2 3 4 5 6 S 9 10 11 12", canonical "M 0 0 C 1 2 3 4 5 6 7 8 9 10 11 12")

[<Fact>]
let ``absolute_smooth_cubic_beziers_parse_test`` () =
    Assert.Equal("M 0 0 C 1 2 3 4 5 6 S 9 10 11 12", canonical "M 0 0 C 1 2 3 4 5 6 S 9 10 11 12")

[<Fact>]
let ``relative_smooth_cubic_beziers_parse_test`` () =
    Assert.Equal("M 10 10 C 11 12 13 14 15 16 S 19 20 21 22", canonical "M 10 10 c 1 2 3 4 5 6 s 4 4 6 6")

[<Fact>]
let ``repeated_smooth_cubic_beziers_parse_test`` () =
    Assert.Equal("M 0 0 C 1 2 3 4 5 6 S 9 10 11 12 S 15 16 17 18", canonical "M 0 0 C 1 2 3 4 5 6 S 9 10 11 12 15 16 17 18")

[<Fact>]
let ``smooth_cubic_without_previous_cubic_uses_current_point_test`` () =
    Assert.Equal("M 0 0 L 5 5 S 10 10 15 15", canonical "M 0 0 L 5 5 S 10 10 15 15")

[<Fact>]
let ``absolute_arcs_parse_test`` () =
    Assert.Equal("M 0 0 A 5 10 30 0 1 20 40", canonical "M 0 0 A 5 10 30 0 1 20 40")

[<Fact>]
let ``relative_arcs_parse_test`` () =
    Assert.Equal("M 10 10 A 5 10 30 1 0 30 50", canonical "M 10 10 a 5 10 30 1 0 20 40")

[<Fact>]
let ``repeated_arcs_parse_test`` () =
    Assert.Equal("M 0 0 A 5 10 30 0 1 20 40 A 7 8 45 1 0 30 50", canonical "M 0 0 A 5 10 30 0 1 20 40 7 8 45 1 0 30 50")

[<Fact>]
let ``repeated_move_coordinates_become_implicit_lines_test`` () =
    Assert.Equal("M 0 0 H 10 V 20", canonical "M0 0 10 0 10 20")

[<Fact>]
let ``relative_move_implicit_lines_stay_relative_test`` () =
    Assert.Equal("M 10 10 H 15 V 15", canonical "m 10 10 5 0 0 5")

[<Fact>]
let ``closepath_adds_closing_line_and_semantic_close_test`` () =
    Assert.Equal("M 0 0 H 10 Z", canonical "M 0 0 L 10 0 z")

[<Fact>]
let ``compact_numbers_parse_test`` () =
    Assert.Equal("M 0 -1 H 10 V 9", canonical "M0-1L10-1V9")

[<Fact>]
let ``comma_separated_coordinate_pairs_parse_test`` () =
    Assert.Equal("M 0 0 L 10 20 L 30 40", canonical "M0,0 L10,20 30,40")

[<Fact>]
let ``generated_valid_coordinate_separator_cases_test`` () =
    let separators = [ " "; "  "; "\t"; "\n"; "\r"; "\u000c"; ","; " , " ]
    let numbers = [ "0"; "-1"; "+2"; ".5"; "-.25"; "1e2"; "2E-1" ]
    for separator in separators do for number in numbers do succeeds ("M" + number + separator + number)

[<Fact>]
let ``generated_valid_signed_number_boundaries_test`` () =
    [ "M0-1"; "M0+1"; "M.5-.25"; "M1e2-2e1"; "M1e-2+3e-4"; "M0 0L1-2-3+4" ] |> List.iter succeeds

[<Fact>]
let ``generated_valid_compact_arc_flag_cases_test`` () =
    for largeArc in [ 0; 1 ] do
        for sweep in [ 0; 1 ] do
            for endpoint in [ "10 20"; "-10-20"; "+10+20" ] do
                succeeds (sprintf "M0 0A5 8 30 %d%d%s" largeArc sweep endpoint)

[<Fact>]
let ``generated_invalid_comma_placement_cases_test`` () =
    [ ",M0 0"; "M,0 0"; "M0,,0"; "M0, ,0"; "M0 0,"; "M0 0,L1 1"; "M0 0 L,1 1"; "M0 0 Z," ] |> List.iter fails

[<Fact>]
let ``generated_invalid_arc_flag_cases_test`` () =
    for flag in [ -3; -2; -1; 2; 3; 4; 5; 6; 7; 8; 9 ] do
        fails (sprintf "M0 0A5 5 0 %d 0 10 10" flag)
        fails (sprintf "M0 0A5 5 0 0 %d 10 10" flag)

let private canonicalCases cases = cases |> List.iter (fun (source, expected) -> Assert.Equal(expected, canonical source))

[<Fact>]
let ``wpt_whitespace_basic_cases_test`` () =
    [ "M 100 100 L 200 200"; "M\t100\t100\tL\t200\t200"; "M\n100\n100\nL\n200\n200"; "M\r100\r100\rL\r200\r200"; "M\u000c100\u000c100\u000cL\u000c200\u000c200"; "M \t\n\r\u000c 100 \t\n\r\u000c 100 \t\n\r\u000c L \t\n\r\u000c 200 \t\n\r\u000c 200"; "   \t\n\r  M 100,100 L 200,200"; "M 100,100 L 200,200   \t\n\r  "; "M100,100L200,200"; "M     100     100     L     200     200"; "M 100 , 100 L 200 , 200"; "M 100,100 L 200,200"; "M 100 ,100 L 200 ,200"; "M 100, 100 L 200, 200" ]
    |> List.iter (fun source -> Assert.Equal("M 100 100 L 200 200", canonical source))

[<Fact>]
let ``wpt_arc_command_syntax_cases_test`` () =
    [ "M 100,100 A 50,50 0 0,1 200,100"; "M 100,100 A 50,50 0 01 200,100"; "M 100,100 A 50,50 0 0 1 200,100" ]
    |> List.iter (fun source -> Assert.Equal("M 100 100 A 50 50 0 0 1 200 100", canonical source))

[<Fact>]
let ``wpt_repeated_arc_arguments_test`` () =
    Assert.Equal("M 50 350 A 25 25 0 0 1 100 350 A 25 25 0 0 1 150 350", canonical "M 50,350 A 25,25 0 0,1 100,350 25,25 0 0,1 150,350")

[<Fact>]
let ``wpt_negative_arc_radius_uses_absolute_value_test`` () =
    Assert.Equal("M 200 300 A 50 50 0 0 1 300 300", canonical "M 200,300 A -50,50 0 0,1 300,300")

[<Fact>]
let ``wpt_zero_arc_radius_becomes_line_test`` () =
    Assert.Equal("M 200 250 H 300", canonical "M 200,250 A 0,0 0 0,1 300,250")

[<Fact>]
let ``svg_same_endpoint_arc_is_omitted_test`` () =
    Assert.Equal("M 20 30", canonical "M 20,30 A 10,10 0 1,1 20,30")

[<Fact>]
let ``wpt_consecutive_signed_number_cases_test`` () =
    canonicalCases [ "M 100-200 L 200-100", "M 100 -200 L 200 -100"; "M 50+100 L 150+200", "M 50 100 L 150 200"; "M 10-20+30-40", "M 10 -20 L 30 -40" ]

[<Fact>]
let ``wpt_consecutive_decimal_number_cases_test`` () =
    canonicalCases [ "M 0.6.5 L 10.5.6", "M 0.6 0.5 L 10.5 0.6"; "M .5.6 L .7.8", "M 0.5 0.6 L 0.7 0.8"; "M 1.2.3.4.5", "M 1.2 0.3 L 0.4 0.5" ]

[<Fact>]
let ``wpt_exponent_number_cases_test`` () =
    canonicalCases [ "M 1e2,1e2 L 2E2,1.5e2", "M 100 100 L 200 150"; "M 1e+2,2e+1", "M 100 20"; "M 1e-1,5e-2", "M 0.1 0.05"; "M 1.5e2,2.5e1", "M 150 25"; "M 5e0,10e0", "M 5 10"; "M 1e2-1e2", "M 100 -100" ]

[<Fact>]
let ``wpt_trailing_decimal_cases_are_rejected_test`` () =
    [ "M 10,10 L 50,50 L 23.,100"; "M 0,0 L 10,10 L 20.,30."; "M 0,0 L 15. 20"; "M 100,100 L 150,100 L 150,150 L 100,150 Z M 200.,200." ] |> List.iter fails

[<Fact>]
let ``wpt_invalid_path_data_cases_are_rejected_test`` () =
    [ "M 10,10 L 50,50 X 100,100"; "M 10,60 L 50,60 L 100"; "M 10,110 L 50,110 60,110 70"; "M 10,160 L 50,160 C 60,150 70,170"; "M 10,210 L 50,210 A 25,25 0 2,1 100,210"; "L 100,260"; "M 10,310 L 50,310 X 60,310 Y 70,310"; "M 0,0 L 50,50 C 60,40 70,60 80,50 C 90,40 100,60"; "M 10,360 L 50,360 L 60 L 100,360"; "M 100,10 x 150,10" ] |> List.iter fails

[<Fact>]
let ``wpt_empty_and_none_path_data_disable_rendering_test`` () =
    Assert.Equal(Ok Path.empty, Parse.path "")
    Assert.Equal(Ok Path.empty, Parse.path "none")

[<Fact>]
let ``wpt_absolute_moveto_cases_test`` () =
    canonicalCases [ "M 100,100", "M 100 100"; "M 50,50 150,50 150,150 50,150 Z", "M 50 50 H 150 V 150 H 50 Z"; "M 10,10 20,20 30,30", "M 10 10 L 20 20 L 30 30"; "M100,100", "M 100 100"; "M 10,10 L 20,20 M 30,30 L 40,40", "M 10 10 L 20 20 M 30 30 L 40 40"; "M 100 , 200", "M 100 200"; "M 100 200", "M 100 200" ]

[<Fact>]
let ``wpt_relative_moveto_cases_test`` () =
    canonicalCases [ "m 100,100 L 150,150", "M 100 100 L 150 150"; "M 50,50 L 100,50 m 0,50 L 150,150", "M 50 50 H 100 M 100 100 L 150 150"; "M 0,0 L 50,0 m 10,10 L 100,50", "M 0 0 H 50 M 60 10 L 100 50"; "m 10,10 20,20 30,30", "M 10 10 L 30 30 L 60 60"; "M 100,100 m -50,-50 L 100,100", "M 100 100 M 50 50 L 100 100"; "M 50,50 m 0,0 L 100,100", "M 50 50 M 50 50 L 100 100"; "M 0,0 L 10,10 m 5,5 m 5,5 L 30,30", "M 0 0 L 10 10 M 15 15 M 20 20 L 30 30" ]

[<Fact>]
let ``wpt_lineto_command_cases_test`` () =
    canonicalCases [ "M 50,50 L 150,150", "M 50 50 L 150 150"; "M 50,50 l 100,100", "M 50 50 L 150 150"; "M 50,200 H 150", "M 50 200 H 150"; "M 200,50 V 150", "M 200 50 V 150"; "M 0,0 L 10,0 20,0 30,0", "M 0 0 H 10 H 20 H 30"; "M 0,50 H 10 20 30", "M 0 50 H 10 H 20 H 30"; "M 50,0 V 10 20 30", "M 50 0 V 10 V 20 V 30"; "M 50,50 h 100", "M 50 50 H 150"; "M 50,50 v 100", "M 50 50 V 150"; "M 100,100 h -50 v -50", "M 100 100 H 50 V 50"; "M 50,50 L 100,50 L 100,100 L 50,100 Z", "M 50 50 H 100 V 100 H 50 Z"; "M 0,0 L 50,0 l 50,0 L 150,0", "M 0 0 H 50 H 100 H 150"; "M 0,0 L 100,0 L 100,100 z", "M 0 0 H 100 V 100 Z" ]

[<Fact>]
let ``closepath_and_explicit_line_home_parse_to_same_subpath_test`` () =
    let direct = Parse.path "M 0 0 L 10 0 Z" |> Result.defaultWith (failwithf "%A")
    let explicit = Parse.path "M 0 0 L 10 0 L 0 0 Z" |> Result.defaultWith (failwithf "%A")
    Assert.Equal(direct.Subpaths |> List.exactlyOne, explicit.Subpaths |> List.exactlyOne)
    Assert.Equal("M 0 0 H 10 Z", Serialize.path direct)

[<Fact>]
let ``commas_parse_between_curve_coordinates_test`` () =
    Assert.Equal("M 0 0 C 1 2 3 4 5 6 Q 7 8 9 10", canonical "M0,0 C1,2,3,4,5,6 Q7,8,9,10")

[<Fact>]
let ``commas_parse_between_arc_arguments_test`` () =
    Assert.Equal("M 0 0 A 25 50 -30 0 1 50 -25", canonical "M0,0 A25,50 -30 0,1 50,-25")

[<Fact>]
let ``exponent_and_plus_signed_numbers_parse_test`` () =
    Assert.Equal("M 10 -20 H 15", canonical "M +1e1 -2E1 L 1.5e1 -2e1")

[<Fact>]
let ``overflowing_path_number_is_rejected_test`` () =
    Assert.Equal(Error(ParseError(InvalidNumber "1e400", "1e400 0")), Parse.path "M 1e400 0")

[<Fact>]
let ``path_exponent_scaling_preserves_finite_compensated_values_test`` () =
    succeeds "M 0.1e309 0"

[<Fact>]
let ``overflowing_path_integer_syntax_is_rejected_test`` () =
    fails ("M " + String.replicate 400 "9" + " 0")

[<Fact>]
let ``large_path_exponents_do_not_require_linear_recursion_test`` () =
    fails "M 1e1000000000 0"
    Assert.Equal("M 0 0", canonical "M 1e-1000000000 0")

[<Fact>]
let ``move_only_subpath_is_preserved_test`` () =
    Assert.Equal("M 0 0", canonical "M 0 0")

[<Fact>]
let ``zero_length_line_subpath_is_not_move_only_test`` () =
    let path = Parse.path "M 0 0 L 0 0" |> Result.defaultWith (failwithf "%A")
    let subpath = path.Subpaths |> List.exactlyOne
    Assert.Single(subpath.Segments) |> ignore
    Assert.Equal("M 0 0 H 0", Serialize.path path)

[<Fact>]
let ``move_only_subpaths_are_ignored_among_real_subpaths_test`` () =
    let path = Parse.path "M 0 0 M 10 10 L 20 10" |> Result.defaultWith (failwithf "%A")
    Assert.Equal("M 10 10 H 20", path.Subpaths |> List.filter (fun s -> not s.Segments.IsEmpty) |> List.exactlyOne |> Serialize.subpath)

[<Fact>]
let ``invalid_arc_flags_are_rejected_test`` () =
    Assert.Equal(Error(ParseError(ExpectedArcFlag, "2 1 10 0")), Parse.path "M 0 0 A 5 5 0 2 1 10 0")

[<Fact>]
let ``concatenated_arc_flags_and_endpoint_parse_test`` () =
    Assert.Equal("M 0 0 A 10 10 0 0 1 10 20", canonical "M0 0A10 10 0 0110 20")

[<Fact>]
let ``every_concatenated_arc_flag_pair_parses_test`` () =
    [ "0010 20"; "0110 20"; "1010 20"; "1110 20" ] |> List.iter (fun args -> succeeds ("M0 0A10 10 0 " + args))

[<Fact>]
let ``concatenated_arc_flags_parse_in_repeated_argument_sets_test`` () =
    Assert.Equal("M 0 0 A 10 10 0 0 1 10 20 A 5 5 0 1 0 -4 -6", canonical "M0 0A10 10 0 0110 20 5 5 0 10-4-6")

[<Fact>]
let ``unsupported_commands_are_rejected_test`` () =
    Assert.Equal(Error(ParseError(UnsupportedCommand "R", "R 1 2 3 4")), Parse.path "M 0 0 R 1 2 3 4")

[<Fact>]
let ``drawing_command_before_move_is_rejected_test`` () =
    Assert.Equal(Error(ParseError(ExpectedMove, "L 10 10")), Parse.path "L 10 10")

[<Fact>]
let ``command_without_required_number_is_rejected_test`` () =
    Assert.Equal(Error(ParseError(ExpectedNumber, "")), Parse.path "M 0 0 L")

[<Fact>]
let ``invalid_number_is_rejected_test`` () =
    Assert.Equal(Error(ParseError(InvalidNumber ".", ". 0")), Parse.path "M . 0")

[<Fact>]
let ``comma_immediately_after_command_is_rejected_test`` () =
    Assert.Equal(Error(ParseError(InvalidSeparator, ",0,0")), Parse.path "M,0,0")

[<Fact>]
let ``repeated_comma_is_rejected_test`` () =
    Assert.Equal(Error(ParseError(InvalidSeparator, ",,0")), Parse.path "M0,,0")

[<Fact>]
let ``trailing_comma_is_rejected_test`` () =
    Assert.Equal(Error(ParseError(InvalidSeparator, ",")), Parse.path "M0 0,")

[<Fact>]
let ``comma_before_command_is_rejected_test`` () =
    Assert.Equal(Error(ParseError(InvalidSeparator, ",L1 1")), Parse.path "M0 0,L1 1")

[<Fact>]
let ``error_remaining_preserves_unicode_suffix_test`` () =
    Assert.Equal(Error(ParseError(UnsupportedCommand "é", "émore")), Parse.path "M0 0 émore")

[<Fact>]
let ``comma_with_surrounding_whitespace_between_numbers_parses_test`` () =
    Assert.Equal("M 0 0 L 1 1", canonical "M 0 , 0 L 1 , 1")

[<Fact>]
let ``svg_form_feed_whitespace_parses_test`` () =
    Assert.Equal("M 0 0 L 1 1", canonical "M\u000c0\u000c0L\u000c1\u000c1")

[<Fact>]
let ``wpt_cubic_curveto_command_cases_test`` () =
    canonicalCases [
        "M 50,50 C 100,25 150,75 200,50", "M 50 50 C 100 25 150 75 200 50"
        "M 50,50 c 50,-25 100,25 150,0", "M 50 50 C 100 25 150 75 200 50"
        "M 0,50 C 25,0 50,0 75,50 100,100 125,100 150,50", "M 0 50 C 25 0 50 0 75 50 S 125 100 150 50"
        "M 50,150 C 75,100 100,100 125,150 S 175,200 200,150", "M 50 150 C 75 100 100 100 125 150 S 175 200 200 150"
        "M 50,50 C 75,25 100,75 125,50 s 50,-25 75,0", "M 50 50 C 75 25 100 75 125 50 S 175 25 200 50"
        "M 50,50 S 100,25 150,50", "M 50 50 S 100 25 150 50"
        "M 0,50 C 25,0 50,0 75,50 S 125,100 150,50 175,0 200,50", "M 0 50 C 25 0 50 0 75 50 S 125 100 150 50 S 175 0 200 50"
        "M 50 50 C 75 25 100 75 125 50", "M 50 50 C 75 25 100 75 125 50"
        "M 50,50 C 75,25,100,75,125,50", "M 50 50 C 75 25 100 75 125 50"
    ]

[<Fact>]
let ``wpt_quadratic_curveto_command_cases_test`` () =
    canonicalCases [
        "M 50,50 Q 100,25 150,50", "M 50 50 Q 100 25 150 50"
        "M 50,50 q 50,-25 100,0", "M 50 50 Q 100 25 150 50"
        "M 0,50 Q 25,25 50,50 75,75 100,50", "M 0 50 Q 25 25 50 50 T 100 50"
        "M 50,150 Q 75,125 100,150 T 150,150", "M 50 150 Q 75 125 100 150 T 150 150"
        "M 50,50 Q 75,25 100,50 t 50,0", "M 50 50 Q 75 25 100 50 T 150 50"
        "M 50,200 T 100,200", "M 50 200 T 100 200"
        "M 0,150 Q 25,125 50,150 T 100,150 150,150", "M 0 150 Q 25 125 50 150 T 100 150 T 150 150"
        "M 0,200 Q 12.5,187.5 25,200 T 50,200 T 75,200", "M 0 200 Q 12.5 187.5 25 200 T 50 200 T 75 200"
        "M 50 250 Q 75 225 100 250", "M 50 250 Q 75 225 100 250"
        "M 50,300 Q 75,275,100,300", "M 50 300 Q 75 275 100 300"
        "M 0,250 C 12.5,237.5 25,237.5 37.5,250 T 75,250", "M 0 250 C 12.5 237.5 25 237.5 37.5 250 T 75 250"
        "M 0,300 Q 12.5,287.5 25,300 T 50,300 Q 62.5,287.5 75,300", "M 0 300 Q 12.5 287.5 25 300 T 50 300 T 75 300"
    ]
