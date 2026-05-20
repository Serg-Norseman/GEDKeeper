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
using System.Threading.Tasks;
using SQLite;

namespace GKcli.Database;


public static class LLMDatabase
{
    private static readonly string SQLiteDB = @"gkrag.db";

    private static string fAppDataPath = string.Empty;
    private static SQLiteAsyncConnection fConnection;

    public static void SetAppDataPath(string path)
    {
        fAppDataPath = path;
    }

    private static void CheckConnection()
    {
        if (fConnection != null) return;

        string dbPath = Path.Combine(fAppDataPath, SQLiteDB);

        if (File.Exists(dbPath)) {
            fConnection = new SQLiteAsyncConnection(dbPath);
            fConnection.ExecuteScalarAsync<string>("PRAGMA journal_mode = WAL;");
            fConnection.ExecuteAsync("PRAGMA auto_vacuum = FULL;");
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

    internal static async Task<IList<ExtractionPattern>> GetPatterns(string century = null)
    {
        CheckConnection();
        if (string.IsNullOrEmpty(century)) {
            return await fConnection.QueryAsync<ExtractionPattern>("select [Id], [RawText], [CorrectedResult], [Embedding], [Century] from [ExtractionPattern]");
        } else {
            return await fConnection.QueryAsync<ExtractionPattern>("select [Id], [RawText], [CorrectedResult], [Embedding], [Century] from [ExtractionPattern] where [Century] = ?", century);
        }
    }

    public static async Task DeletePattern(int id)
    {
        CheckConnection();
        await fConnection.ExecuteAsync("delete from [ExtractionPattern] where [Id] = ?", id);
    }

    /*public static void UpdatePattern(int id, string inputText, string embedding, string correctedResult, string century)
    {
        CheckConnection();

        fConnection.Execute("update [ExtractionPattern] set [RawText] = ?, [CorrectedResult] = ?, [Embedding] = ?, [Century] = ? where [Id] = ?", inputText, correctedResult, embedding, century, id);
    }*/

    public static async Task WritePattern(string inputText, byte[] embedding, string correctedResult, string century)
    {
        CheckConnection();

        var pattern = new ExtractionPattern() {
            RawText = inputText,
            Embedding = embedding,
            CorrectedResult = correctedResult,
            Century = century
        };
        await fConnection.InsertAsync(pattern);

        //fConnection.Execute("insert into [ExtractionPattern] ([RawText], [CorrectedResult], [Embedding], [Century]) values (?, ?, ?, ?)", inputText, correctedResult, embedding, century);
    }

    public static async Task<(int totalPatterns, IList<string> uniqueCenturies)> GetPatternStats()
    {
        CheckConnection();
        var totalCount = await fConnection.ExecuteScalarAsync<int>("select count(*) from [ExtractionPattern]");
        var uniqueCenturies = await fConnection.QueryScalarsAsync<string>("select distinct [Century] from [ExtractionPattern] where [Century] is not null and [Century] != ''");
        return (totalCount, uniqueCenturies);
    }

    public static string ExportPatternsToJson()
    {
        var patterns = GetPatterns(null);
        return JsonSerializer.Serialize(patterns, new JsonSerializerOptions { WriteIndented = true });
    }

    #endregion

    #region Memory Entries

    internal static async Task<IList<MemoryEntry>> GetMemoryEntries()
    {
        CheckConnection();
        return await fConnection.QueryAsync<MemoryEntry>("select [Content], [Embedding] from [MemoryEntry]");
    }

    public static async Task WriteMemoryEntry(MemoryEntry entry)
    {
        CheckConnection();
        await fConnection.InsertAsync(entry);
    }

    #endregion

    #region Context

    /// <summary>
    /// Extract context summarization.
    /// </summary>
    internal static async Task<AssistantSummary> GetSummary(string sessionId)
    {
        CheckConnection();
        return await fConnection.Table<AssistantSummary>().Where(s => s.SessionId == sessionId).FirstOrDefaultAsync();
    }

    internal static async Task InsertSummary(AssistantSummary summary)
    {
        CheckConnection();
        await fConnection.InsertAsync(summary);
    }

    internal static async Task UpdateSummary(AssistantSummary summary)
    {
        CheckConnection();
        await fConnection.UpdateAsync(summary);
    }

    #endregion

    #region Profile

    /// <summary>
    /// Extract user profile and preferences
    /// </summary>
    internal static async Task<List<UserPreference>> GetUserPreferences()
    {
        CheckConnection();
        return await fConnection.Table<UserPreference>().ToListAsync();
    }

    internal static async Task<UserPreference> GetPreference(string normalizedKey)
    {
        CheckConnection();
        return await fConnection.Table<UserPreference>().Where(p => p.PrefKey == normalizedKey).FirstOrDefaultAsync();
    }

    internal static async Task InsertPreference(UserPreference newPref)
    {
        CheckConnection();
        await fConnection.InsertAsync(newPref);
    }

    internal static async Task UpdatePreference(UserPreference existingPref)
    {
        CheckConnection();
        await fConnection.UpdateAsync(existingPref);
    }

    internal static async Task DeletePreference(UserPreference existing)
    {
        CheckConnection();
        await fConnection.DeleteAsync(existing);
    }

    #endregion

    #region TaskBoard

    /// <summary>
    /// Extract active research tasks (Blackboard)
    /// </summary>
    internal static async Task<List<AssistantTask>> GetActiveTasks()
    {
        CheckConnection();
        return await fConnection.Table<AssistantTask>().Where(t => t.Status == "ACTIVE").ToListAsync();
    }

    internal static async Task<AssistantTask> GetTask(int taskId)
    {
        CheckConnection();
        return await fConnection.Table<AssistantTask>().Where(t => t.TaskId == taskId).FirstOrDefaultAsync();
    }

    internal static async Task InsertTask(AssistantTask value)
    {
        CheckConnection();
        await fConnection.InsertAsync(value);
    }

    internal static async Task UpdateTask(AssistantTask value)
    {
        CheckConnection();
        await fConnection.UpdateAsync(value);
    }

    #endregion

    #region Graph

    internal static async Task<GraphEntity> GetEntity(string normalizedId)
    {
        CheckConnection();
        return await fConnection.Table<GraphEntity>().Where(e => e.EntityId == normalizedId).FirstOrDefaultAsync();
    }

    internal static async Task InsertEntity(GraphEntity entity)
    {
        CheckConnection();
        await fConnection.InsertAsync(entity);
    }

    internal static async Task UpdateEntity(GraphEntity entity)
    {
        CheckConnection();
        await fConnection.UpdateAsync(entity);
    }

    internal static async Task<GraphRelation> GetRelation(string src, string trg, string pred)
    {
        CheckConnection();
        return await fConnection.Table<GraphRelation>().Where(r => r.SourceEntityId == src && r.Predicate == pred && r.TargetEntityId == trg).FirstOrDefaultAsync();
    }

    internal static async Task<List<GraphRelation>> GetRelationBySource(string normalizedId)
    {
        CheckConnection();
        return await fConnection.Table<GraphRelation>().Where(r => r.SourceEntityId == normalizedId).ToListAsync();
    }

    internal static async Task<List<GraphRelation>> GetRelationByTarget(string normalizedId)
    {
        CheckConnection();
        return await fConnection.Table<GraphRelation>().Where(r => r.TargetEntityId == normalizedId).ToListAsync();
    }

    internal static async Task InsertRelation(GraphRelation relation)
    {
        CheckConnection();
        await fConnection.InsertAsync(relation);
    }

    internal static async Task UpdateRelation(GraphRelation relation)
    {
        CheckConnection();
        await fConnection.UpdateAsync(relation);
    }

    #endregion
}
