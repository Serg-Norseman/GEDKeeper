/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Runtime.CompilerServices;
using System.Text;

namespace GKUI.Platform;

/* to tests
string bb = "[b]Title[/b]\n[size=+1]Subtitle[/size]\n[size=+3]Main Section[/size]\n[url=https://example.com]Click[/url]";
string md = BbCodeConverter.ToMarkdown(bb);
Output:
**Title**
###### Subtitle
### Main Section
[Click](https://example.com)
*/

public static class BbCodeConverter
{
    /// <summary>
    /// Converts BBCode to Markdown with maximum throughput.
    /// Supported tags: b, i, u, s, code, quote, url, img, ul/ol, li, h1-h6, size=+N
    /// </summary>
    /// <param name="input">BBCode input text</param>
    /// <returns>Markdown formatted string</returns>
    public static string ToMarkdown(ReadOnlySpan<char> input)
    {
        // Pre-allocate ~1.8x input length to minimize internal buffer reallocations
        var sb = new StringBuilder(input.Length * 2);
        int i = 0;
        int len = input.Length;

        // Lightweight state for [url=href]...[/url] handling
        string? pendingHref = null;

        while (i < len) {
            // Fast path: regular characters
            if (input[i] != '[') {
                sb.Append(input[i]);
                i++;
                continue;
            }

            // Locate closing bracket of the current tag
            int close = input[i..].IndexOf(']');
            if (close < 0) { sb.Append(input[i]); i++; continue; }
            close += i;

            bool isClosing = close > i + 1 && input[i + 1] == '/';
            int nameStart = isClosing ? i + 2 : i + 1;
            int eqPos = input[nameStart..close].IndexOf('=');
            int nameEnd = eqPos >= 0 ? nameStart + eqPos : close;

            var tagName = input[nameStart..nameEnd];

            // Inline-style tags
            if (IsTag(tagName, "b")) { sb.Append("**"); i = close + 1; continue; }
            if (IsTag(tagName, "i")) { sb.Append("*"); i = close + 1; continue; }
            if (IsTag(tagName, "u")) { sb.Append("__"); i = close + 1; continue; }
            if (IsTag(tagName, "s")) { sb.Append("~~"); i = close + 1; continue; }
            if (IsTag(tagName, "code")) { sb.Append("```"); i = close + 1; continue; }
            if (IsTag(tagName, "quote")) { sb.Append(isClosing ? "\n" : "> "); i = close + 1; continue; }

            // URL handling: [url=href]text[/url] -> [text](href)
            if (IsTag(tagName, "url")) {
                if (isClosing) {
                    if (pendingHref != null) {
                        sb.Append("](").Append(pendingHref).Append(')');
                        pendingHref = null;
                    } else {
                        // Fallback for [url]link[/url] -> assumes link equals href
                        sb.Append("](");
                    }
                    i = close + 1;
                    continue;
                }

                if (eqPos >= 0) {
                    int st = nameStart + eqPos + 1;
                    pendingHref = input[st..close].ToString();
                }
                sb.Append('[');
                i = close + 1;
                continue;
            }

            // Image handling
            if (IsTag(tagName, "img")) {
                sb.Append(isClosing ? ")" : "![](");
                i = close + 1;
                continue;
            }

            // Lists
            if (IsTag(tagName, "ul") || IsTag(tagName, "ol")) { i = close + 1; continue; }
            if (IsTag(tagName, "li")) { sb.Append(isClosing ? "" : "- "); i = close + 1; continue; }

            // Explicit headings: [h1]..[h6]
            if (tagName.Length == 2 && tagName[0] == 'h' && tagName[1] >= '1' && tagName[1] <= '6') {
                if (isClosing) { i = close + 1; continue; }
                int level = tagName[1] - '0';
                sb.Append('\n');
                for (int k = 0; k < level; k++) sb.Append('#');
                sb.Append(' ');
                i = close + 1;
                continue;
            }

            // Relative size mapping: [size=+N] where N=1..6 -> h6..h1
            if (IsTag(tagName, "size")) {
                if (isClosing) { i = close + 1; continue; }

                if (eqPos >= 0) {
                    int st = nameStart + eqPos + 1;
                    var val = input[st..close];
                    // Strict format check: exactly "+1" to "+6"
                    if (val.Length == 2 && val[0] == '+' && val[1] >= '1' && val[1] <= '6') {
                        int n = val[1] - '0';
                        int hashes = 7 - n; // +1 -> 6 hashes (h6), +2 -> 5 (h5) ... +6 -> 1 (h1)
                        sb.Append('\n');
                        for (int k = 0; k < hashes; k++) sb.Append('#');
                        sb.Append(' ');
                        i = close + 1;
                        continue;
                    }
                }

                // Fallback: unsupported size value, preserve original tag
                sb.Append(input[i]); i++;
                continue;
            }

            // Unknown tag: preserve as-is to avoid data loss
            sb.Append(input[i]);
            i++;
        }

        return sb.ToString();
    }

    /// <summary>
    /// Allocation-free, case-insensitive tag comparison.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool IsTag(ReadOnlySpan<char> span, string value) =>
        span.Equals(value, StringComparison.OrdinalIgnoreCase);
}
