/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Linq;
using ZLMKit.MCP;

namespace GKMCPPlugin.Utilities;

public class QueryEqualizer : IQueryEqualizer
{
    private static readonly string[] UpsertTokens = new string[] { "add", "edit", "create", "update" };

    public void AdjustQueryLimit(ref string[] queryTokens, ref int limit)
    {
        // Increase limit based on RecordTypeMap terms found in query
        // For each RecordTypeMap term found (beyond the first one), increase limit by 3
        int adjustedLimit = limit;

        int recordTypeMatches = 0;
        foreach (var keyTok in RuntimeData.RecordTypeMap.Keys) {
            if (queryTokens.Any(token => token.Equals(keyTok, StringComparison.OrdinalIgnoreCase))) {
                recordTypeMatches++;
            }
        }

        int upsertMatches = 0;
        foreach (var keyTok in UpsertTokens) {
            if (queryTokens.Any(token => token.Equals(keyTok, StringComparison.OrdinalIgnoreCase))) {
                upsertMatches++;
            }
        }

        if (recordTypeMatches > 0 && upsertMatches > 0) {
            adjustedLimit += recordTypeMatches * 3;

            int len = queryTokens.Length;
            Array.Resize(ref queryTokens, len + 1);
            queryTokens[len] = "upsert";
        }

        limit = adjustedLimit;
    }
}
