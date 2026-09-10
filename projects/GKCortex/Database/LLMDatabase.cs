/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.IO;
using System.Text.Json;
using SQLite;

namespace GKCortex.Database;


public static class LLMDatabase
{
    private static readonly string SQLiteDB = @"gkrag.db";

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

                conn.CreateTable<AssistantSummary>();
                conn.CreateTable<AssistantTask>();
                conn.CreateTable<GraphEntity>();
                conn.CreateTable<GraphRelation>();
                conn.CreateTable<UserPreference>();
            }
            CheckConnection();
        }
    }

    #region Patterns

    internal static IList<ExtractionPattern> GetPatterns(string century = null)
    {
        CheckConnection();
        if (string.IsNullOrEmpty(century)) {
            return fConnection.Query<ExtractionPattern>("select [id], [raw_text], [corrected_result], [embedding], [century] from [extraction_patterns]");
        } else {
            return fConnection.Query<ExtractionPattern>("select [id], [raw_text], [corrected_result], [embedding], [century] from [extraction_patterns] where [century] = ?", century);
        }
    }

    public static void DeletePattern(int id)
    {
        CheckConnection();
        fConnection.Execute("delete from [extraction_patterns] where [Id] = ?", id);
    }

    /*public static void UpdatePattern(int id, string inputText, string embedding, string correctedResult, string century)
    {
        CheckConnection();

        fConnection.Execute("update [extraction_patterns] set [raw_text] = ?, [corrected_result] = ?, [embedding] = ?, [century] = ? where [Id] = ?", inputText, correctedResult, embedding, century, id);
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

        //fConnection.Execute("insert into [extraction_patterns] ([raw_text], [corrected_result], [embedding], [century]) values (?, ?, ?, ?)", inputText, correctedResult, embedding, century);
    }

    public static (int totalPatterns, IList<string> uniqueCenturies) GetPatternStats()
    {
        CheckConnection();
        var totalCount = fConnection.ExecuteScalar<int>("select count(*) from [extraction_patterns]");
        var uniqueCenturies = fConnection.QueryScalars<string>("select distinct [century] from [extraction_patterns] where [century] is not null and [century] != ''");
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
        return fConnection.Query<MemoryEntry>("select [content], [embedding] from [memory_entries]");
    }

    public static void WriteMemoryEntry(MemoryEntry entry)
    {
        CheckConnection();
        fConnection.Insert(entry);
    }

    #endregion

    #region Context

    /// <summary>
    /// Extract context summarization.
    /// </summary>
    internal static AssistantSummary GetSummary(string sessionId)
    {
        CheckConnection();
        return fConnection.Table<AssistantSummary>().Where(s => s.SessionId == sessionId).FirstOrDefault();
    }

    internal static void InsertSummary(AssistantSummary summary)
    {
        CheckConnection();
        fConnection.Insert(summary);
    }

    internal static void UpdateSummary(AssistantSummary summary)
    {
        CheckConnection();
        fConnection.Update(summary);
    }

    #endregion

    #region Profile

    /// <summary>
    /// Extract user profile and preferences
    /// </summary>
    internal static List<UserPreference> GetUserPreferences()
    {
        CheckConnection();
        return fConnection.Table<UserPreference>().ToList();
    }

    internal static UserPreference GetPreference(string normalizedKey)
    {
        CheckConnection();
        return fConnection.Table<UserPreference>().Where(p => p.PrefKey == normalizedKey).FirstOrDefault();
    }

    internal static void InsertPreference(UserPreference newPref)
    {
        CheckConnection();
        fConnection.Insert(newPref);
    }

    internal static void UpdatePreference(UserPreference existingPref)
    {
        CheckConnection();
        fConnection.Update(existingPref);
    }

    internal static void DeletePreference(UserPreference existing)
    {
        CheckConnection();
        fConnection.Delete(existing);
    }

    #endregion

    #region TaskBoard

    /// <summary>
    /// Extract active research tasks (Blackboard)
    /// </summary>
    internal static List<AssistantTask> GetActiveTasks()
    {
        CheckConnection();
        return fConnection.Table<AssistantTask>().Where(t => t.Status == "ACTIVE").ToList();
    }

    internal static AssistantTask GetTask(int taskId)
    {
        CheckConnection();
        return fConnection.Table<AssistantTask>().Where(t => t.TaskId == taskId).FirstOrDefault();
    }

    internal static void InsertTask(AssistantTask value)
    {
        CheckConnection();
        fConnection.Insert(value);
    }

    internal static void UpdateTask(AssistantTask value)
    {
        CheckConnection();
        fConnection.Update(value);
    }

    #endregion

    #region Graph

    internal static GraphEntity GetEntity(string normalizedId)
    {
        CheckConnection();
        return fConnection.Table<GraphEntity>().Where(e => e.EntityId == normalizedId).FirstOrDefault();
    }

    internal static void InsertEntity(GraphEntity entity)
    {
        CheckConnection();
        fConnection.Insert(entity);
    }

    internal static void UpdateEntity(GraphEntity entity)
    {
        CheckConnection();
        fConnection.Update(entity);
    }

    internal static GraphRelation GetRelation(string src, string trg, string pred)
    {
        CheckConnection();
        return fConnection.Table<GraphRelation>().Where(r => r.SourceEntityId == src && r.Predicate == pred && r.TargetEntityId == trg).FirstOrDefault();
    }

    internal static List<GraphRelation> GetRelationBySource(string normalizedId)
    {
        CheckConnection();
        return fConnection.Table<GraphRelation>().Where(r => r.SourceEntityId == normalizedId).ToList();
    }

    internal static List<GraphRelation> GetRelationByTarget(string normalizedId)
    {
        CheckConnection();
        return fConnection.Table<GraphRelation>().Where(r => r.TargetEntityId == normalizedId).ToList();
    }

    internal static void InsertRelation(GraphRelation relation)
    {
        CheckConnection();
        fConnection.Insert(relation);
    }

    internal static void UpdateRelation(GraphRelation relation)
    {
        CheckConnection();
        fConnection.Update(relation);
    }

    #endregion
}
