/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using GKCore.Utilities;

namespace GKcli.MCP;

/// <summary>
/// Tool Discovery & Execution Pattern.
/// Supported: `search_tool` and `use_tool`.
/// </summary>
internal static class MCPToolDiscovery
{
    private record ToolMetadata(MCPTool Tool, string[] NameTags, string[] DescTags);

    private record SearchResult(MCPTool Tool, double Score);


    // Weights for different parts of metadata
    private const double NameWeight = 1.0;
    private const double TagWeight = 0.8;
    private const double DescriptionWeight = 0.4;


    private static readonly Dictionary<string, MCPTool> _tools = new();
    private static readonly List<ToolMetadata> _registry = new();


    public static void Register(string name, MCPTool tool)
    {
        _tools[tool.Name] = tool;

        _registry.Add(new ToolMetadata(
            Tool: tool,
            NameTags: Extract(tool.Name, ""),
            DescTags: Extract("", tool.Description)
        ));
    }

    public static IEnumerable<MCPTool> Search(string query, int limit = 5)
    {
        var queryTokens = PrepareTokens(query);
        if (queryTokens.Length == 0)
            return new List<MCPTool>();

        return _registry
            .Select(tool => new SearchResult(tool.Tool, CalculateScore(queryTokens, tool)))
            .Where(r => r.Score > 0.1) // Garbage cutoff threshold
            .OrderByDescending(r => r.Score)
            .Take(limit)
            .Select(r => r.Tool)
            .ToList();
    }

    private static double CalculateScore(string[] queryTokens, ToolMetadata tool)
    {
        double score = 0;

        // Search by name (most accurate)
        score += MatchTokens(queryTokens, tool.NameTags) * NameWeight;

        // Search by other tags
        //score += MatchTokens(queryTokens, tool.Tags.SelectMany(PrepareTokens).ToArray()) * TagWeight;

        // Search by description
        score += MatchTokens(queryTokens, tool.DescTags) * DescriptionWeight;

        return score;
    }

    private static string[] PrepareTokens(string input)
    {
        return input.ToLowerInvariant()
            .Split(new[] { ' ', '_', '-', '.', ',' }, StringSplitOptions.RemoveEmptyEntries)
            .Select(Stem)
            .Where(s => s.Length > 2) // Ignore prepositions
            .ToArray();
    }

    private static double MatchTokens(string[] queryTokens, string[] targetTokens)
    {
        if (targetTokens.Length == 0) return 0;

        double matches = 0;
        foreach (var q in queryTokens) {
            foreach (var t in targetTokens) {
                if (t == q) matches += 1.0; // A complete match
                else if (t.Contains(q) || q.Contains(t)) matches += 0.5; // Partial
                else if (SysUtils.Levenshtein(q, t) <= 1) matches += 0.3; // Typo
            }
        }
        return matches / targetTokens.Length;
    }

    #region Stemmer

    // Regular expressions for the Russian language
    private static readonly Regex PerfectiveGerund = new Regex("((ив|ивши|ившись)|(([ая])(в|вши|вшись)))$", RegexOptions.Compiled);
    private static readonly Regex Adjective = new Regex("(ее|ие|ые|ое|ими|ыми|ей|ий|ый|ой|ем|им|ым|ом|его|ого|ему|ому|их|ых|ую|юю|ая|яя|ою|ею)$", RegexOptions.Compiled);
    private static readonly Regex Verb = new Regex("((ила|ыла|ена|ейте|ите|ишь|еть|ить|ыть|нно)|(([ая])(ла|на|ете|йте|ли|й|л|н|ло|но|ет|ют|ны|ть|ешь|нно)))$", RegexOptions.Compiled);
    private static readonly Regex Noun = new Regex("(а|ев|ов|ие|ье|е|иями|ями|ами|еи|ии|и|ией|ей|ой|ий|й|иям|ям|ием|ем|ам|ом|о|у|ах|иях|ях|ы|ь|ию|ью|ю|ия|ья|я)$", RegexOptions.Compiled);
    private static readonly Regex Reflexive = new Regex("(ся|сь)$", RegexOptions.Compiled);

    private static string Stem(string word)
    {
        word = word.ToLowerInvariant().Replace('ё', 'е');

        // For English words, we use the simplest logic (remove 's', 'ing', 'ed')
        if (Regex.IsMatch(word, "^[a-z]+$"))
            return StemEnglish(word);

        // For Russian words
        string original = word;
        word = Reflexive.Replace(word, "");
        var temp = PerfectiveGerund.Replace(word, "");
        if (temp == word) {
            word = Adjective.Replace(word, "");
            temp = Verb.Replace(word, "");
            if (temp == word) word = Noun.Replace(word, "");
            else word = temp;
        } else word = temp;

        return word.Length > 2 ? word : original;
    }

    private static string StemEnglish(string word)
    {
        if (word.EndsWith("ies") && word.Length > 4) return word[..^3] + "i";
        if (word.EndsWith("es") && word.Length > 3) return word[..^2];
        if (word.EndsWith("s") && word.Length > 2 && !word.EndsWith("ss")) return word[..^1];
        if (word.EndsWith("ing") && word.Length > 5) return word[..^3];
        if (word.EndsWith("ed") && word.Length > 4) return word[..^2];
        return word;
    }

    #endregion

    #region Tag Extractor

    // A list of words that carry no semantic meaning
    private static readonly HashSet<string> StopWords = new()
    {
        "tool", "method", "helper", "database", "execute", "perform", "with", "from"
    };

    // Genealogical thesaurus for normalization (synonyms)
    private static readonly Dictionary<string, string> Synonyms = new()
    {
        { "relatives", "family" },
        { "ancestor", "lineage" },
        { "birth", "event" },
        { "death", "event" },
        { "location", "place" },
        { "marriage", "union" }
    };

    private static string[] Extract(string toolName, string description)
    {
        var tags = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

        // Splitting the tool name (search_person_by_event -> search, person, by, event)
        var nameTokens = SplitSnakeCase(toolName);
        foreach (var token in nameTokens) tags.Add(token.ToLowerInvariant());

        // Extracting nouns and verbs from the description
        var descTokens = description.ToLowerInvariant()
            .Split(new[] { ' ', '.', ',', ';', '(', ')' }, StringSplitOptions.RemoveEmptyEntries)
            .Where(t => t.Length > 3 && !StopWords.Contains(t));
        foreach (var token in descTokens) tags.Add(token);

        // Adding normalized synonyms
        var synonymTags = new List<string>();
        foreach (var tag in tags) {
            if (Synonyms.TryGetValue(tag, out var synonym))
                synonymTags.Add(synonym);
        }
        foreach (var s in synonymTags) tags.Add(s);

        return tags.Select(Stem).Distinct().ToArray();
    }

    private static string[] SplitCamelCase(string input)
    {
        // "SearchPersonByEvent" -> "Search Person By Event"
        return Regex.Replace(input, "([A-Z])", " $1", RegexOptions.Compiled).Trim().Split(' ');
    }

    private static string[] SplitSnakeCase(string input)
    {
        // "search_person_by_event" -> "search person by event"
        return input.Trim().Split('_', StringSplitOptions.RemoveEmptyEntries);
    }

    #endregion
}
