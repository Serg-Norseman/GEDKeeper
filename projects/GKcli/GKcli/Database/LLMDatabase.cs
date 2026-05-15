/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.Json;
using SQLite;

namespace GKcli.Database;


public static class LLMDatabase
{
    private static readonly string SQLiteDB = @"gkrag.db";
    //private const string TblExtractionPatternSQL = "create table [ExtractionPattern] ([Id] integer not null primary key autoincrement, [RawText] text not null, [CorrectedResult] text not null, [Embedding] text not null, [Century] text not null default '')";

    private static string fAppDataPath = string.Empty;
    private static SQLiteConnection fConnection;

    public static void SetAppDataPath(string path)
    {
        fAppDataPath = path;
    }

    private static void CheckConnection()
    {
        if (fConnection != null) return;

        string dbPath = Path.Combine(fAppDataPath, SQLiteDB);

        if (File.Exists(dbPath)) {
            fConnection = new SQLiteConnection(dbPath);
            fConnection.ExecuteScalar<string>("PRAGMA journal_mode = WAL;");
            fConnection.Execute("PRAGMA auto_vacuum = FULL;");
        } else {
            using (var conn = new SQLiteConnection(dbPath)) {
                //conn.BeginTransaction();
                //conn.Execute(TblExtractionPatternSQL);
                //conn.Commit();

                conn.CreateTable<ExtractionPattern>();
                conn.CreateTable<MemoryEntry>();
            }
            CheckConnection();
        }
    }

    #region Patterns

    internal static IList<ExtractionPattern> GetPatterns(string century = null)
    {
        CheckConnection();
        if (string.IsNullOrEmpty(century)) {
            return fConnection.Query<ExtractionPattern>("select [Id], [RawText], [CorrectedResult], [Embedding], [Century] from [ExtractionPattern]");
        } else {
            return fConnection.Query<ExtractionPattern>("select [Id], [RawText], [CorrectedResult], [Embedding], [Century] from [ExtractionPattern] where [Century] = ?", century);
        }
    }

    public static void DeletePattern(int id)
    {
        CheckConnection();
        fConnection.Execute("delete from [ExtractionPattern] where [Id] = ?", id);
    }

    /*public static void UpdatePattern(int id, string inputText, string embedding, string correctedResult, string century)
    {
        CheckConnection();

        fConnection.Execute("update [ExtractionPattern] set [RawText] = ?, [CorrectedResult] = ?, [Embedding] = ?, [Century] = ? where [Id] = ?", inputText, correctedResult, embedding, century, id);
    }*/

    public static void WritePattern(string inputText, byte[] embedding, string correctedResult, string century)
    {
        CheckConnection();

        var pattern = new ExtractionPattern() {
            RawText = inputText,
            Embedding = embedding,
            CorrectedResult = correctedResult,
            Century = century
        };
        fConnection.Insert(pattern);

        //fConnection.Execute("insert into [ExtractionPattern] ([RawText], [CorrectedResult], [Embedding], [Century]) values (?, ?, ?, ?)", inputText, correctedResult, embedding, century);
    }

    public static (int totalPatterns, IList<string> uniqueCenturies) GetPatternStats()
    {
        CheckConnection();
        var totalCount = fConnection.ExecuteScalar<int>("select count(*) from [ExtractionPattern]");
        var uniqueCenturies = fConnection.QueryScalars<string>("select distinct [Century] from [ExtractionPattern] where [Century] is not null and [Century] != ''").ToList();
        return (totalCount, uniqueCenturies);
    }

    public static string ExportPatternsToJson()
    {
        var patterns = GetPatterns(null);
        return JsonSerializer.Serialize(patterns, new JsonSerializerOptions { WriteIndented = true });
    }

    #endregion

    #region Memory Entries

    internal static IList<MemoryEntry> GetMemoryEntries()
    {
        CheckConnection();
        return fConnection.Query<MemoryEntry>("select [Content], [Embedding] from [MemoryEntry]");
    }

    public static void WriteMemoryEntry(MemoryEntry entry)
    {
        CheckConnection();
        fConnection.Insert(entry);
    }

    #endregion
}
