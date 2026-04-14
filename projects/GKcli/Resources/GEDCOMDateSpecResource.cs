/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using GKcli.MCP;
using GKCore;

namespace GKcli.Resources;

internal class GEDCOMDateSpecResource : BaseResource
{
    public GEDCOMDateSpecResource() : base(@"gedcom://date_spec") { }

    public override MCPResource CreateResource()
    {
        return new MCPResource {
            Uri = @"gedcom://date_spec",
            Name = "date_spec",
            Description = "GEDCOM date specification",
            MimeType = "text/plain"
        };
    }

    public override List<MCPResourceContents> Get(BaseContext baseContext)
    {
        return new List<MCPResourceContents> {
            new MCPResourceContents {
                Uri = fUri,
                MimeType = "text/plain",
                Text = GetSpec()
            }
        };
    }

    private static string GetSpec()
    {
        // Parser accepts BC, BCE and B.C.

        return @"
Condensed GEDCOM Date Spec.
Format: [Modifier] [Day] [Month] [Year] [Suffix].
1. Basic parts:
- Months (strictly 3 letters EN): JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC.
- Year: 3-4 digits. Add suffix BC if needed.
- Day: 1-2 digits (1–31).
- Any part may be missing.
2. Certainty modifiers (only one of the acceptable ones or missing):
- Exact (without modifier): [Day] [Month] [Year] (e.g., 12 MAY 1850).
- Approximated (about, calculated, estimated): ABT [Date], CAL [Date], EST [Date].
- Range (event happened in one moment): AFT [Date], BEF [Date], BET [Date] AND [Date].
- Period (event occurred during the period): FROM [Date], TO [Date], FROM [Date] TO [Date].
3. Restrictions:
- Output only the date string. No explanations, no quotes.
- If data is missing/unparseable, return (UNKNOWN).
- Forbidden: commas, slashes, dots, dashes, or lowercase.
- Order strict: Day Month Year (if present).
- Non-standard: ([Text]) — text in parentheses if date is not formalized.
- Nested modifiers are not allowed.
";
    }
}
