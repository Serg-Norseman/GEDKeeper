/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Text;
using System.Text.Json;
using GDModel;
using GKCore.Options;

namespace GKcli.MCP;

public static class MCPHelper
{
    private const int pageSize = 20;

    internal delegate string RowBuilder(int index);

    internal static List<MCPContent> PageableTable(string tableName, JsonElement args, int recordCount, RowBuilder buildRow)
    {
        if (recordCount == 0)
            return MCPContent.CreateSimpleContent($"No {tableName} in database.");

        int page = GetIntArgument(args, "page", 1);
        if (page < 1) page = 1;

        int totalPages = (recordCount + pageSize - 1) / pageSize;
        if (page > totalPages) page = totalPages;

        int startIndex = (page - 1) * pageSize;
        int endIndex = Math.Min(startIndex + pageSize, recordCount);

        var lines = new StringBuilder();
        lines.Append($"{tableName} (page {page}/{totalPages}, {startIndex + 1}-{endIndex} of {recordCount}):\n");
        lines.Append(buildRow(-1)); // header
        for (int i = startIndex; i < endIndex; i++) {
            var line = buildRow(i);
            if (string.IsNullOrEmpty(line)) continue;

            lines.Append("\n");
            lines.Append(line);
        }
        if (page < totalPages) {
            lines.Append($"\n\n_Next page: use parameter page={page + 1}_");
        }

        MCPServer.Log($"Successfully generated response for {recordCount} rows (page {page}/{totalPages})");

        return MCPContent.CreateSimpleContent(lines.ToString());
    }

    public static string ToUpperFirst(string s) 
    {
        return string.IsNullOrEmpty(s) ? s : char.ToUpper(s[0]) + s.Substring(1);
    }

    internal static string GetDateValue(GDMCustomDate date)
    {
        string result;

        if (date == null) {
            result = string.Empty;
        } else {
            GlobalOptions glob = GlobalOptions.Instance;
            result = date.GetDisplayString(glob.DefDateFormat, glob.ShowDatesSign, glob.ShowDatesCalendar);
        }

        return result;
    }

    internal static string GetRequiredArgument(JsonElement args, string argName)
    {
        if (!args.TryGetProperty(argName, out var argElem) || argElem.ValueKind != JsonValueKind.String)
            throw new ArgumentException($"Missing required argument: {argName}");

        return argElem.GetString()!;
    }

    internal static int GetIntArgument(JsonElement args, string argName, int defaultValue)
    {
        if (!args.TryGetProperty(argName, out var argElem) || argElem.ValueKind != JsonValueKind.Number)
            return defaultValue;

        return argElem.GetInt32();
    }

    internal static string GetStringArgument(JsonElement args, string argName, string defaultValue)
    {
        if (!args.TryGetProperty(argName, out var argElem) || argElem.ValueKind != JsonValueKind.String)
            return defaultValue;

        return argElem.GetString()!;
    }

    internal static double GetDoubleArgument(JsonElement args, string argName, double defaultValue)
    {
        if (!args.TryGetProperty(argName, out var argElem) || argElem.ValueKind != JsonValueKind.Number)
            return defaultValue;

        return argElem.GetDouble();
    }

    internal static bool GetBoolArgument(JsonElement args, string argName, bool defaultValue)
    {
        if (!args.TryGetProperty(argName, out var argElem) || argElem.ValueKind != JsonValueKind.True && argElem.ValueKind != JsonValueKind.False)
            return defaultValue;

        return argElem.GetBoolean();
    }
}
