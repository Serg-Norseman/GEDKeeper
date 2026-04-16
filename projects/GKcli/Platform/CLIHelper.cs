/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.IO;
using System.Text.RegularExpressions;
using System.Web;

namespace GKUI.Platform;

// FIXME: I need to come up with a more suitable name.
internal static class CLIHelper
{
    public static string ToUpperFirst(string s)
    {
        return string.IsNullOrEmpty(s) ? s : char.ToUpper(s[0]) + s.Substring(1);
    }

    public static string GetPlainText(TextReader htmlReader)
    {
        var doc = new HtmlAgilityPack.HtmlDocument();
        doc.Load(htmlReader);

        var nodesToRemove = doc.DocumentNode.SelectNodes("//script|//style|//header|//footer|//nav");
        if (nodesToRemove != null) {
            foreach (var node in nodesToRemove) node.Remove();
        }

        string text = doc.DocumentNode.InnerText;
        text = HttpUtility.HtmlDecode(text);
        return text.Trim();
    }

    public static string StripRtf(string rtf)
    {
        // nuget RtfPipe
        // return GetPlainText(Rtf.ToHtml(rtf).Replace("<br>", "\n"));

        // temporary primitive solution
        if (string.IsNullOrEmpty(rtf)) return "";
        return Regex.Replace(rtf, @"\{\*?\\[^{}]+?\}|\\([a-z?]+(-?\d+)?|[\s'""\\])", "", RegexOptions.IgnoreCase).Trim();
    }
}
