/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;
using GKcli.Database;
using GKcli.RAG;

namespace GKcli.Memory;

internal class MemoryService
{
    private const string LocalModel = "llama3";
    private readonly string _localLmLlapiUrl; // e.g., "http://localhost:11434/api/generate" for Ollama

    private readonly HttpClient _httpClient;
    private readonly int _tokenThresholdChars; // Trigger threshold in characters (~4-5k characters)

    public MemoryService(string localLmUrl = "http://localhost:11434/api/generate", int charThreshold = 5000)
    {
        _httpClient = new HttpClient { Timeout = TimeSpan.FromSeconds(30) };
        _localLmLlapiUrl = localLmUrl;
        _tokenThresholdChars = charThreshold;
    }

    #region Context

    /// <summary>
    /// Aggregates all memory layers into a single text block for injection into the LM context
    /// </summary>
    public async Task<string> GetInjectedContextAsync(string sessionId)
    {
        var sb = new StringBuilder();
        sb.AppendLine("=== ASSISTANT OFFLINE MEMORY (HARD CONTEXT) ===");
        AssistantSummary summary = await LLMDatabase.GetSummary(sessionId);

        sb.AppendLine("[CHRONOLOGY AND BRIEF DIALOGUE CONTENT]:");
        if (summary != null) {
            sb.AppendLine($"- Summary of past sessions: {summary.GlobalSummary}");
            sb.AppendLine($"- Current session context: {summary.CurrentSessionSummary}");
        } else {
            sb.AppendLine("- No past sessions. This is the beginning of the interaction.");
        }
        sb.AppendLine();

        var preferences = await LLMDatabase.GetUserPreferences();
        sb.AppendLine("[USER PROFILE AND PREFERENCES]:");
        if (preferences.Count > 0) {
            foreach (var pref in preferences) {
                sb.AppendLine($"- {pref.PrefKey}: {pref.PrefValue} (Confidence: {pref.ConfidenceScore:F1})");
            }
        } else {
            sb.AppendLine("- Profile is empty. Specific preferences have not yet been identified.");
        }
        sb.AppendLine();

        var activeTasks = await LLMDatabase.GetActiveTasks();
        sb.AppendLine("[ACTIVE GENEALOGICAL TASKS IN PROGRESS]:");
        if (activeTasks.Count > 0) {
            foreach (var task in activeTasks) {
                sb.AppendLine($"* TASK ID: {task.TaskId}");
                sb.AppendLine($"  Target Person: {task.TargetPerson}");
                sb.AppendLine($"  Search Objective: {task.GoalDescription}");

                // Format JSON arrays back into readable lists
                var checkedSources = RAGHelper.DeserializeJsonList(task.CheckedSourcesJson);
                sb.AppendLine($"  Already checked: {(checkedSources.Length > 0 ? string.Join(", ", checkedSources) : "nothing checked yet")}");

                var nextSteps = RAGHelper.DeserializeJsonList(task.NextStepsJson);
                sb.AppendLine($"  Planned steps: {(nextSteps.Length > 0 ? string.Join(" -> ", nextSteps) : "not defined")}");
            }
        } else {
            sb.AppendLine("- No active tasks. Any new complex user request should initiate task creation.");
        }
        sb.AppendLine("=================================================");

        return sb.ToString();
    }

    /// <summary>
    /// Adds new dialogue turns to the current session and compresses them if necessary.
    /// </summary>
    public async Task AppendAndOptimizeContextAsync(string sessionId, string newUserMessage, string assistantResponse)
    {
        // 1. Get current record from DB
        var summary = await LLMDatabase.GetSummary(sessionId);

        if (summary == null) {
            summary = new AssistantSummary {
                SessionId = sessionId,
                GlobalSummary = "Start of new genealogical research.",
                CurrentSessionSummary = $"User: {newUserMessage}\nAssistant: {assistantResponse}\n",
                LastUpdated = DateTime.UtcNow
            };
            await LLMDatabase.InsertSummary(summary);
            return;
        }

        // 2. Append new turns to current context
        summary.CurrentSessionSummary += $"User: {newUserMessage}\nAssistant: {assistantResponse}\n";
        summary.LastUpdated = DateTime.UtcNow;

        // 3. Check if context compression is needed (length-based estimate for simplicity in offline solutions)
        if (summary.CurrentSessionSummary.Length > _tokenThresholdChars) {
            // Launch summarization. To avoid making the user wait for MCP response,
            // in a real server this could be run via Task.Run without await (fire-and-forget),
            // but with mandatory error handling inside.
            _ = Task.Run(async () => {
                try {
                    await CompressContextAsync(summary);
                } catch (Exception ex) {
                    // Your logger should go here (Console.Error.WriteLine for MCP)
                    Console.Error.WriteLine($"❌ Background context compression failed: {ex.Message}");
                }
            });
        } else {
            await LLMDatabase.UpdateSummary(summary);
        }
    }

    /// <summary>
    /// Calls the local model to transfer the current session into global memory
    /// </summary>
    private async Task CompressContextAsync(AssistantSummary summary)
    {
        // Formulate a strict system prompt for a weaker model to prevent hallucinations
        string compressPrompt = $@"You are the background memory compression module for a genealogical assistant. 
Your task is to merge the old global summary and new dialogue turns into ONE concise, fact-dense paragraph.
Ignore pleasantries, greetings, and filler. Write strictly facts, names, dates, and social estates.

OLD GLOBAL SUMMARY:
{summary.GlobalSummary}

NEW DIALOGUE TURNS FROM CURRENT SESSION:
{summary.CurrentSessionSummary}

Output the new merged global summary in English. It must contain ALL key chronological information. Do not write anything except the summary.";

        // Send request to local model (example based on Ollama API structure)
        var requestBody = new {
            model = LocalModel, // or qwen2.5 / mistral, as installed by the user
            prompt = compressPrompt,
            stream = false,
            options = new { temperature = 0.1 } // Minimal temperature for factual accuracy
        };

        var jsonRequest = JsonSerializer.Serialize(requestBody);
        var content = new StringContent(jsonRequest, Encoding.UTF8, "application/json");

        var response = await _httpClient.PostAsync(_localLmLlapiUrl, content);
        if (response.IsSuccessStatusCode) {
            var jsonResponse = await response.Content.ReadAsStringAsync();
            using var doc = JsonDocument.Parse(jsonResponse);

            // Extract response text (in Ollama the field is called "response")
            if (doc.RootElement.TryGetProperty("response", out var responseElement)) {
                string newGlobalSummary = responseElement.GetString()?.Trim() ?? string.Empty;

                if (!string.IsNullOrEmpty(newGlobalSummary)) {
                    // Update state in DB: 
                    // Clear current session (or keep last 2 lines), and consolidate global memory.
                    summary.GlobalSummary = newGlobalSummary;
                    summary.CurrentSessionSummary = "Context cleared after archival. Dialogue continues from this point.\n";
                    summary.LastUpdated = DateTime.UtcNow;

                    await LLMDatabase.UpdateSummary(summary);
                }
            }
        }
    }

    #endregion

    #region Profile

    /// <summary>
    /// Adds or updates a user setting/preference in upsert mode.
    /// </summary>
    public async Task<bool> SetPreferenceAsync(string key, string value, double confidenceScore = 1.0)
    {
        if (string.IsNullOrWhiteSpace(key)) return false;

        // Normalize key to lowercase to avoid duplicates due to case variations from small models
        string normalizedKey = key.Trim().ToLowerInvariant();
        var existingPref = await LLMDatabase.GetPreference(normalizedKey);

        if (existingPref != null) {
            // If value is the same, just update date and confidence if it's higher
            existingPref.PrefValue = value.Trim();
            existingPref.ConfidenceScore = Math.Max(existingPref.ConfidenceScore, confidenceScore);
            existingPref.LastUpdated = DateTime.UtcNow;
            await LLMDatabase.UpdatePreference(existingPref);
        } else {
            var newPref = new UserPreference {
                PrefKey = normalizedKey,
                PrefValue = value.Trim(),
                ConfidenceScore = confidenceScore,
                LastUpdated = DateTime.UtcNow
            };
            await LLMDatabase.InsertPreference(newPref);
        }

        return true;
    }

    /// <summary>
    /// Returns all preferences as a convenient dictionary.
    /// </summary>
    public async Task<Dictionary<string, string>> GetAllPreferencesAsync()
    {
        var list = await LLMDatabase.GetUserPreferences();
        var result = new Dictionary<string, string>();

        foreach (var item in list) {
            result[item.PrefKey] = item.PrefValue;
        }

        return result;
    }

    /// <summary>
    /// Removes a specific key from the profile if the preference is no longer relevant.
    /// </summary>
    public async Task<bool> DeletePreferenceAsync(string key)
    {
        string normalizedKey = key.Trim().ToLowerInvariant();
        var existing = await LLMDatabase.GetPreference(normalizedKey);

        if (existing == null) return false;
        await LLMDatabase.DeletePreference(existing);
        return true;
    }

    #endregion

    #region Task Board

    /// <summary>
    /// Creates a new genealogical research task.
    /// </summary>
    public async Task<int> CreateTaskAsync(string targetPerson, string goalDescription)
    {
        var task = new AssistantTask {
            TargetPerson = targetPerson?.Trim() ?? string.Empty,
            GoalDescription = goalDescription?.Trim() ?? string.Empty,
            Status = "ACTIVE",
            CheckedSourcesJson = "[]",
            NextStepsJson = "[]",
            CreatedAt = DateTime.UtcNow
        };

        await LLMDatabase.InsertTask(task);
        return task.TaskId; // sqlite-net automatically populates ID after insert
    }

    /// <summary>
    /// Adds a verified source and/or overwrites the plan for next steps.
    /// </summary>
    public async Task<bool> UpdateTaskProgressAsync(int taskId, string newCheckedSource, string[] newNextSteps)
    {
        var task = await LLMDatabase.GetTask(taskId);

        if (task == null) return false;

        // 1. Update checked sources (if a new one is provided)
        if (!string.IsNullOrWhiteSpace(newCheckedSource)) {
            var sources = JsonSerializer.Deserialize<List<string>>(task.CheckedSourcesJson) ?? new List<string>();
            string normalizedSource = newCheckedSource.Trim();

            if (!sources.Contains(normalizedSource)) {
                sources.Add(normalizedSource);
                task.CheckedSourcesJson = JsonSerializer.Serialize(sources);
            }
        }

        // 2. Overwrite next steps (plan is always dynamic)
        if (newNextSteps != null) {
            task.NextStepsJson = JsonSerializer.Serialize(newNextSteps);
        }

        await LLMDatabase.UpdateTask(task);
        return true;
    }

    /// <summary>
    /// Changes task status (COMPLETED, PAUSED, ACTIVE)
    /// </summary>
    public async Task<bool> ChangeTaskStatusAsync(int taskId, string newStatus)
    {
        var task = await LLMDatabase.GetTask(taskId);

        if (task == null) return false;

        string status = newStatus?.Trim().ToUpperInvariant() ?? "ACTIVE";
        if (status == "ACTIVE" || status == "COMPLETED" || status == "PAUSED") {
            task.Status = status;
            await LLMDatabase.UpdateTask(task);
            return true;
        }

        return false;
    }

    #endregion

    #region Graph

    /// <summary>
    /// Adds or updates a node in the knowledge graph
    /// </summary>
    public async Task AddEntityAsync(string id, string name, string type, string description = "")
    {
        var normalizedId = id.Trim().ToLowerInvariant();
        var existing = await LLMDatabase.GetEntity(normalizedId);

        if (existing != null) {
            existing.Name = name.Trim();
            existing.Type = type.Trim().ToUpperInvariant();
            if (!string.IsNullOrEmpty(description)) existing.Description = description.Trim();
            await LLMDatabase.UpdateEntity(existing);
        } else {
            await LLMDatabase.InsertEntity(new GraphEntity {
                EntityId = normalizedId,
                Name = name.Trim(),
                Type = type.Trim().ToUpperInvariant(),
                Description = description.Trim()
            });
        }
    }

    /// <summary>
    /// Creates a directed relationship between two graph nodes
    /// </summary>
    public async Task AddRelationAsync(string sourceId, string predicate, string targetId, string notes = "")
    {
        var src = sourceId.Trim().ToLowerInvariant();
        var trg = targetId.Trim().ToLowerInvariant();
        var pred = predicate.Trim().ToUpperInvariant();

        // Check for duplicate relationships to prevent graph clutter
        var existing = await LLMDatabase.GetRelation(src, trg, pred);

        if (existing == null) {
            await LLMDatabase.InsertRelation(new GraphRelation {
                SourceEntityId = src,
                Predicate = pred,
                TargetEntityId = trg,
                ContextNotes = notes.Trim()
            });
        }
    }

    /// <summary>
    /// Extracts the ego-network (entity and all its first-order connections) for the local LM
    /// </summary>
    public async Task<string> GetLocalSubGraphAsTextAsync(string entityId)
    {
        var normalizedId = entityId.Trim().ToLowerInvariant();

        var mainEntity = await LLMDatabase.GetEntity(normalizedId);
        if (mainEntity == null) return $"❌ Entity with ID '{entityId}' not found in knowledge base.";

        var sb = new StringBuilder();
        sb.AppendLine($"--- LOCAL KNOWLEDGE SUBGRAPH: {mainEntity.Name} ({mainEntity.Type}) ---");
        if (!string.IsNullOrEmpty(mainEntity.Description)) sb.AppendLine($"Description: {mainEntity.Description}");
        sb.AppendLine("RELATIONS AND CONTEXTUAL LANDSCAPE:");

        // Outgoing relations (From this node)
        var outgoing = await LLMDatabase.GetRelationBySource(normalizedId);
        foreach (var rel in outgoing) {
            var target = await LLMDatabase.GetEntity(rel.TargetEntityId);
            string targetName = target != null ? $"{target.Name} [{target.Type}]" : rel.TargetEntityId;
            string notes = string.IsNullOrEmpty(rel.ContextNotes) ? "" : $" ({rel.ContextNotes})";
            sb.AppendLine($"  => [{mainEntity.Name}] --({rel.Predicate})--> [{targetName}]{notes}");
        }

        // Incoming relations (To this node)
        var incoming = await LLMDatabase.GetRelationByTarget(normalizedId);
        foreach (var rel in incoming) {
            var source = await LLMDatabase.GetEntity(rel.SourceEntityId);
            string sourceName = source != null ? $"{source.Name} [{source.Type}]" : rel.SourceEntityId;
            string notes = string.IsNullOrEmpty(rel.ContextNotes) ? "" : $" ({rel.ContextNotes})";
            sb.AppendLine($"  <= [{sourceName}] --({rel.Predicate})--> [{mainEntity.Name}]{notes}");
        }

        return sb.ToString();
    }

    #endregion
}
