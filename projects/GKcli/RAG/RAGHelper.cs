/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text.Json;
using SmartComponents.LocalEmbeddings;
using SQLite;

namespace GKcli.RAG;


/// <summary>
/// Helper class for RAG (Retrieval-Augmented Generation).
/// </summary>
internal static class RAGHelper
{
    private static readonly NumberFormatInfo fNumberFormat;
    private static readonly Dictionary<string, (EmbeddingF32 Vector, DateTime Timestamp)> fEmbeddingsCache = new();

    static RAGHelper()
    {
        fNumberFormat = new NumberFormatInfo();
        fNumberFormat.NumberDecimalSeparator = ".";
    }

    public static string SearchExamples(string inputText, string century = null, int topK = 10)
    {
        /*
        For archaic spelling, pure semantic search is insufficient. A hybrid search and post-ranking are needed.

        // Semantic search (current approach)
        var semanticMatches = await SearchByEmbeddingAsync(inputText, century, topK * 2);
    
        // Fuzzy matching keywords (for spelling variations)
        var keywordMatches = await SearchByFuzzyKeywordsAsync(inputText, century, topK * 2);
    
        // Reciprocal Rank Fusion to combine results
        var fused = ReciprocalRankFusion(semanticMatches, keywordMatches, k=60);
    
        // Post-ranking using meta-features
        var reranked = fused.OrderByDescending(m => 
            m.Score * 
            CenturyRelevanceWeight(m.Pattern.Century, century) * 
            OrthographySimilarity(inputText, m.Pattern.RawText) // heuristics for old spelling
        ).Take(topK).ToList();
    
        return reranked;
        */

        // Obtain a vector for the new census text
        var inputVector = GetCachedEmbedding(inputText);
        var inputVectorArr = inputVector.Values.ToArray();

        // Extract patterns from database
        var patterns = GetPatterns(century);

        // Count the similarities (-> EmbeddingF32.Similarity())
        var bestMatches = patterns
            .Select(p => new {
                Pattern = p,
                Score = CosineSimilarity(inputVectorArr, GetVector(p.Embedding))
            }).OrderByDescending(x => x.Score).Take(topK).ToList();

        // Forming a context for the MCP server
        string examples = $@"<rag_examples century=""{century}"">
<instruction>
Use the following examples as a template for parsing historical text.
Please note:
- archaic word forms (do not correct them in the output)
- census record structure (name, age, class, locality)
- spelling features of the specified century
</instruction>

{string.Join("\n\n", bestMatches.Select((m, i) => $@"
<example id=""{i+1}"" score=""{m.Score:F3}"">
<input>{m.Pattern.RawText}</input>
<output>{m.Pattern.CorrectedResult}</output>
</example>"))}

<guidance>
{(bestMatches.Average(m => m.Score) < 0.5
    ? "⚠️ Examples with low similarity were found. Pay particular attention to deviations in structure."
    : "✅ The examples are relevant. Follow their structure.")}
</guidance>
</rag_examples>";

        return examples;
    }

    private static readonly TimeSpan CacheTTL = TimeSpan.FromHours(2);
    private static readonly int MaxCacheSize = 500;

    private static EmbeddingF32 GetCachedEmbedding(string text)
    {
        if (fEmbeddingsCache.TryGetValue(text, out var cached) && DateTime.UtcNow - cached.Timestamp < CacheTTL)
            return cached.Vector;

        var embedding = new LocalEmbedder().Embed(text);

        // LRU overflow eviction
        if (fEmbeddingsCache.Count >= MaxCacheSize) {
            var oldest = fEmbeddingsCache.OrderBy(kv => kv.Value.Timestamp).First().Key;
            fEmbeddingsCache.Remove(oldest);
        }

        fEmbeddingsCache[text] = (embedding, DateTime.UtcNow);
        return embedding;
    }

    public static void ClearEmbeddingsCache()
    {
        fEmbeddingsCache.Clear();
    }

    public static void WritePattern(string inputText, string correctedResult, string century)
    {
        CheckConnection();

        var embeddingModel = new LocalEmbedder();
        var inputVector = embeddingModel.Embed(inputText);
        var embedding = SetVector(inputVector.Values.ToArray());

        fConnection.Execute("insert into [ExtractionPattern] ([RawText], [CorrectedResult], [Embedding], [Century]) values (?, ?, ?, ?)", inputText, correctedResult, embedding, century);
    }

    #region Patterns database

    private sealed class ExtractionPattern
    {
        public int Id { get; set; }

        // Original text from the census (archaic)
        public string RawText { get; set; } = string.Empty;

        // Ideal parsing result (JSON or structured text)
        public string CorrectedResult { get; set; } = string.Empty;

        // Vector of this text (embedding)
        // Will be stored in the database as a string "0.12;0.45;..." or BLOB
        public string Embedding { get; set; } = string.Empty;

        // Additional filters (century, region) to narrow your search
        public string Century { get; set; } = string.Empty;
    }


    private static readonly string SQLiteDB = @"gkrag.db";
    private const string TblExtractionPatternSQL = "create table [ExtractionPattern] ([Id] integer not null primary key autoincrement, [RawText] text not null, [CorrectedResult] text not null, [Embedding] text not null, [Century] text not null default '')";

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
                conn.BeginTransaction();
                conn.Execute(TblExtractionPatternSQL);
                conn.Commit();
            }
            CheckConnection();
        }
    }

    private static IList<ExtractionPattern> GetPatterns(string century = null)
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

    public static void UpdatePattern(int id, string inputText, string correctedResult, string century)
    {
        CheckConnection();

        var embeddingModel = new LocalEmbedder();
        var inputVector = embeddingModel.Embed(inputText);
        var embedding = SetVector(inputVector.Values.ToArray());

        fConnection.Execute("update [ExtractionPattern] set [RawText] = ?, [CorrectedResult] = ?, [Embedding] = ?, [Century] = ? where [Id] = ?", inputText, correctedResult, embedding, century, id);
    }

    public static (int totalPatterns, IList<string> uniqueCenturies) GetDatabaseStats()
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

    #region Utilities

    private static string SetVector(float[] values)
    {
        var strValues = values.Select(x => x.ToString(fNumberFormat));
        return string.Join(';', strValues);
    }

    private static float[] GetVector(string embedding)
    {
        var values = embedding.Split(';', StringSplitOptions.RemoveEmptyEntries);
        var vector = new float[values.Length];
        for (int i = 0; i < values.Length; i++) {
            vector[i] = float.Parse(values[i], fNumberFormat);
        }
        return vector;
    }

    // Store both versions of the pattern: the original and the normalized one.
    // Calculate the embedding for the normalized one,
    // but return the original one to the context—this will preserve authenticity for LLM.
    private static string NormalizeHistoricalOrthography(string text)
    {
        // Примеры правил для русского языка 17–19 вв.:
        return text
            .Replace("ѣ", "е").Replace("ѳ", "ф").Replace("і", "и")  // ять, фита, и десятеричное
            .Replace("ъ", "").Replace("ѵ", "и")                      // ер на конце, ижица
            .ToLowerInvariant();
    }

    #endregion

    #region Similarity

    public static float CosineSimilarity(float[] vectorA, float[] vectorB)
    {
        if (vectorA.Length != vectorB.Length)
            throw new ArgumentException("Vectors must be the same length");

        float dotProduct = 0f;
        float magnitudeA = 0f;
        float magnitudeB = 0f;
        for (int i = 0; i < vectorA.Length; i++) {
            dotProduct += vectorA[i] * vectorB[i];
            magnitudeA += vectorA[i] * vectorA[i];
            magnitudeB += vectorB[i] * vectorB[i];
        }
        float divisor = (float)(Math.Sqrt(magnitudeA) * Math.Sqrt(magnitudeB));
        return divisor == 0 ? 0 : dotProduct / divisor;
    }


    #endregion
}
