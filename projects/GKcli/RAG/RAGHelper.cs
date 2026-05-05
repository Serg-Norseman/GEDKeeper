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
using SmartComponents.LocalEmbeddings;
using SQLite;

namespace GKcli.RAG;


/// <summary>
/// Helper class for RAG (Retrieval-Augmented Generation).
/// </summary>
internal static class RAGHelper
{
    internal static NumberFormatInfo numberFormat;

    static RAGHelper()
    {
        numberFormat = new NumberFormatInfo();
        numberFormat.NumberDecimalSeparator = ".";
    }

    public static string SearchExamples(string inputText, string century)
    {
        // Obtain a vector for the new census text
        var embeddingModel = new LocalEmbedder();
        var inputVector = embeddingModel.Embed(inputText);
        var inputVectorArr = inputVector.Values.ToArray();

        // Extract patterns from database
        var patterns = GetPatterns(century);

        // Count the similarities (-> EmbeddingF32.Similarity())
        var bestMatches = patterns
            .Select(p => new {
                Pattern = p,
                Score = CosineSimilarity(inputVectorArr, GetVector(p.Embedding))
            }).OrderByDescending(x => x.Score).Take(3).ToList();

        // Forming a context for the MCP server
        string examples = "Use these examples to process text:\n" + string.Join("\n", bestMatches.Select(m =>
            $"Text: {m.Pattern.RawText} => Result: {m.Pattern.CorrectedResult}"));

        return examples;
    }

    public static void WritePattern(string inputText, string correctedResult, string century)
    {
        CheckConnection();

        var embeddingModel = new LocalEmbedder();
        var inputVector = embeddingModel.Embed(inputText);
        var embedding = SetVector(inputVector.Values.ToArray());

        _connection.Execute($"insert into [ExtractionPattern] ([RawText], [CorrectedResult], [Embedding], [Century]) values ('{inputText}', '{correctedResult}', '{embedding}', '{century}')");
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


    private static readonly string SQLITE_DB = @"gkrag.db";
    private const string TblExtractionPatternSQL = "create table [ExtractionPattern] ([Id] integer not null primary key autoincrement, [RawText] text not null, [CorrectedResult] text not null, [Embedding] text not null, [Century] text not null)";

    private static SQLiteConnection _connection;
    private static string _appDataPath = string.Empty;

    public static void SetAppDataPath(string path)
    {
        _appDataPath = path;
    }

    private static void CheckConnection()
    {
        if (_connection != null) return;

        string dbPath = Path.Combine(_appDataPath, SQLITE_DB);

        if (File.Exists(dbPath)) {
            _connection = new SQLiteConnection(dbPath);
            _connection.ExecuteScalar<string>("PRAGMA journal_mode = WAL;");
            _connection.Execute("PRAGMA auto_vacuum = FULL;");
        } else {
            using (var conn = new SQLiteConnection(dbPath)) {
                conn.BeginTransaction();
                conn.Execute(TblExtractionPatternSQL);
                conn.Commit();
            }
            CheckConnection();
        }
    }

    private static IList<ExtractionPattern> GetPatterns(string century)
    {
        CheckConnection();
        return _connection.Query<ExtractionPattern>("select [RawText], [CorrectedResult], [Embedding] from [ExtractionPattern] where [Century] = ?", century);
    }

    #endregion

    #region Utilities

    private static string SetVector(float[] values)
    {
        var strValues = values.Select(x => x.ToString(numberFormat));
        return string.Join(';', strValues);
    }

    private static float[] GetVector(string embedding)
    {
        var values = embedding.Split(';', StringSplitOptions.RemoveEmptyEntries);
        var vector = new float[values.Length];
        for (int i = 0; i < values.Length; i++) {
            vector[i] = float.Parse(values[i], numberFormat);
        }
        return vector;
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
