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
using System.Linq;
using System.Text.Json;
using System.Threading.Tasks;
using GKCortex.Database;
using SmartComponents.LocalEmbeddings;

namespace GKCortex.RAG;


internal class SearchResult<T> where T : IEntity
{
    public int Id;
    public T Entity;
    public double Score;
}


internal class PatternSearchResult : SearchResult<ExtractionPattern> { }


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

    /// <summary>
    /// Reciprocal Rank Fusion (RRF).
    /// </summary>
    /// <returns></returns>
    private static List<PatternSearchResult> ReciprocalRankFusion(List<PatternSearchResult> semanticMatches, List<PatternSearchResult> keywordMatches, double k = 60.0)
    {
        var result = new List<PatternSearchResult>();

        var rrfScores = new Dictionary<int, PatternSearchResult>();

        for (int index = 0; index < semanticMatches.Count; index++) {
            var match = semanticMatches[index];
            match.Score = 1.0 / (k + (index + 1));
            rrfScores[match.Id] = match;
        }

        for (int index = 0; index < keywordMatches.Count; index++) {
            var match = keywordMatches[index];
            var rank = 1.0 / (k + (index + 1));

            if (rrfScores.TryGetValue(match.Id, out PatternSearchResult rrfMath)) {
                rrfMath.Score += rank;
            } else {
                match.Score = rank;
                rrfScores[match.Id] = match;
            }
        }

        return result;
    }

    const bool DEBUG_OPT = true;

    public static async Task<string> SearchExamples(string inputText, string century = null, int topK = 10)
    {
        List<PatternSearchResult> bestMatches;

        if (DEBUG_OPT) {
            // For archaic spelling, pure semantic search is insufficient. A hybrid search and post-ranking are needed.

            // Semantic search (current approach)
            var semanticMatches = await SearchByEmbeddingAsync(inputText, century, topK * 2);

            // Fuzzy matching keywords (for spelling variations)
            var keywordMatches = await SearchByFuzzyKeywordsAsync(inputText, century, topK * 2);

            // Reciprocal Rank Fusion to combine results
            var fused = ReciprocalRankFusion(semanticMatches, keywordMatches);

            // Post-ranking using meta-features
            bestMatches = fused.OrderByDescending(m =>
                m.Score
            /* * CenturyRelevanceWeight(m.Pattern.Century, century)
            * OrthographySimilarity(inputText, m.Pattern.RawText) // heuristics for old spelling */
            ).Take(topK).ToList();
        } else {
            bestMatches = await SearchByEmbeddingAsync(inputText, century, topK);
        }

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
<example id=""{i + 1}"" score=""{m.Score:F3}"">
<input>{m.Entity.RawText}</input>
<output>{m.Entity.CorrectedResult}</output>
</example>"))}

<guidance>
{(bestMatches.Average(m => m.Score) < 0.5
    ? "⚠️ Examples with low similarity were found. Pay particular attention to deviations in structure."
    : "✅ The examples are relevant. Follow their structure.")}
</guidance>
</rag_examples>";

        return examples;
    }

    /// <summary>
    /// Semantic search (current approach).
    /// </summary>
    private static async Task<List<PatternSearchResult>> SearchByEmbeddingAsync(string inputText, string century, int topK)
    {
        // Obtain a vector for the new census text
        var inputVector = GetCachedEmbedding(inputText);

        // Extract patterns from database
        var patterns = await LLMDatabase.GetPatterns(century);

        // Count the similarities
        var bestMatches = patterns
            .Select(p => new PatternSearchResult { Id = p.Id, Entity = p, Score = inputVector.Similarity(new EmbeddingF32(p.Embedding)) })
            .OrderByDescending(x => x.Score).Take(topK).ToList();

        /*var bestMatches = patterns
            .Select(p => new { Pattern = p, Score = CosineSimilarity(inputVector, GetVector(p.Embedding)) })
            .OrderByDescending(x => x.Score).Take(topK).ToList();*/

        return bestMatches;
    }

    private static async Task<List<PatternSearchResult>> SearchByFuzzyKeywordsAsync(string inputText, string century, int topK)
    {
        var result = new List<PatternSearchResult>();

        /*string formattedQuery = string.Join(" ", inputText.Split(' ', StringSplitOptions.RemoveEmptyEntries).Select(w => w + "*"));
        var ftsRankedIds = LLMDatabase.Query<Fact>(@"
            SELECT f.Id 
            FROM Fact f
            JOIN FactFTS fts ON f.Id = fts.rowid
            WHERE FactFTS MATCH ?
            ORDER BY bm25(fts) ASC 
            LIMIT ?", formattedQuery, topK)
        .Select(f => f.Id)
        .ToList();*/

        return await Task.FromResult(result); // temp
    }

    private static readonly TimeSpan CacheTTL = TimeSpan.FromHours(2);
    private static readonly int MaxCacheSize = 500;

    internal static EmbeddingF32 GetCachedEmbedding(string text)
    {
        if (fEmbeddingsCache.TryGetValue(text, out var cached) && DateTime.UtcNow - cached.Timestamp < CacheTTL)
            return cached.Vector;

        var embedding = Embed(text);

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

    public static async Task WritePattern(string inputText, string correctedResult, string century)
    {
        //var embedding = SetVector(Embed(inputText));
        var embedding = Embed(inputText).Buffer.ToArray();
        await LLMDatabase.WritePattern(inputText, embedding, correctedResult, century);
    }

    #region Utilities

    private static LocalEmbedder fEmbedder = null;

    internal static EmbeddingF32 Embed(string inputText, int maximumTokens = 512)
    {
        if (fEmbedder == null) {
            fEmbedder = new LocalEmbedder();
        }
        var embedding = fEmbedder.Embed(inputText, maximumTokens)/*.Values.ToArray()*/;
        return embedding;
    }

    /*private static string SetVector(float[] values)
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

    private static float CosineSimilarity(float[] vectorA, float[] vectorB)
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
    }*/

    public static string[] DeserializeJsonList(string json)
    {
        try {
            return JsonSerializer.Deserialize<string[]>(json) ?? Array.Empty<string>();
        } catch {
            return Array.Empty<string>();
        }
    }

    #endregion
}
