/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.IO;
using System.Text.Json;
using System.Threading.Tasks;
using GKCortex.Protocols;

namespace GKCortex.LMChat
{
    public sealed class Session
    {
        public string Id { get; set; }

        public string Title { get; set; }

        public DateTime StartDate { get; set; }
        public DateTime? EndDate { get; set; }

        public string ModelId { get; set; }

        public double Temperature { get; set; }
        public double TopP { get; set; }
        public double PresencePenalty { get; set; }
        public double FrequencyPenalty { get; set; }
        public int MaxTokens { get; set; }

        public string SystemPrompt { get; set; }

        public override string ToString()
        {
            return Title;
        }
    }

    public class LMHistoryStorage
    {
        private string fBasePath;
        private List<ChatMessage> fCurrentHistory;
        private string fCurrentSessionId;
        private readonly Dictionary<string, Session> fSessions;

        public string BasePath
        {
            get {
                return fBasePath;
            }
            set {
                fBasePath = value;
                if (!Directory.Exists(fBasePath)) {
                    Directory.CreateDirectory(fBasePath);
                }
                ReloadSessions();
            }
        }

        public List<ChatMessage> CurrentHistory
        {
            get { return fCurrentHistory; }
        }

        public string CurrentSessionId
        {
            get { return fCurrentSessionId; }
            private set {
                fCurrentSessionId = value;
            }
        }

        public IReadOnlyDictionary<string, Session> Sessions
        {
            get { return fSessions; }
        }


        public LMHistoryStorage()
        {
            fSessions = new Dictionary<string, Session>();
            fCurrentHistory = new List<ChatMessage>();

            fBasePath = string.Empty;
            fCurrentSessionId = null;
        }

        private void ReloadSessions()
        {
            fSessions.Clear();

            var directories = Directory.GetDirectories(fBasePath);
            foreach (var dir in directories) {
                var dirName = Path.GetFileName(dir);
                var metadataFile = Path.Combine(dir, "metadata.json");

                if (File.Exists(metadataFile)) {
                    try {
                        var json = File.ReadAllText(metadataFile);
                        var metadata = JsonSerializer.Deserialize<Session>(json);
                        fSessions[dirName] = metadata;
                    } catch {
                        // Skip directories with invalid metadata
                        continue;
                    }
                }
            }
        }

        public async Task LoadSessionAsync(string sessionId)
        {
            if (string.IsNullOrEmpty(sessionId) || !fSessions.ContainsKey(sessionId))
                throw new ArgumentException("Invalid chat ID");

            var chatPath = Path.Combine(fBasePath, sessionId);
            var historyFile = Path.Combine(chatPath, "history.json");

            if (!File.Exists(historyFile)) {
                fCurrentHistory = new List<ChatMessage>();
                fCurrentSessionId = sessionId;
                return;
            }

            try {
                var json = await File.ReadAllTextAsync(historyFile);
                var options = new JsonSerializerOptions { WriteIndented = true };
                fCurrentHistory = JsonSerializer.Deserialize<List<ChatMessage>>(json, options) ?? new List<ChatMessage>();
                fCurrentSessionId = sessionId;
            } catch {
                fCurrentHistory = new List<ChatMessage>();
                fCurrentSessionId = sessionId;
            }
        }

        public async Task SaveCurrentSessionAsync()
        {
            if (string.IsNullOrEmpty(fCurrentSessionId))
                throw new InvalidOperationException("No chat is currently loaded");

            var chatPath = Path.Combine(fBasePath, fCurrentSessionId);
            if (!Directory.Exists(chatPath)) {
                Directory.CreateDirectory(chatPath);
            }

            var historyFile = Path.Combine(chatPath, "history.json");
            var options = new JsonSerializerOptions { WriteIndented = true };
            var json = JsonSerializer.Serialize(fCurrentHistory, options);
            await File.WriteAllTextAsync(historyFile, json);
        }

        public void CreateNewSession(string title, LMSettings settings, string systemPrompt)
        {
            var sessionId = Guid.NewGuid().ToString("N");
            var chatPath = Path.Combine(fBasePath, sessionId);
            Directory.CreateDirectory(chatPath);

            var metadata = new Session {
                Id = sessionId,
                Title = title,
                StartDate = DateTime.Now,
                ModelId = settings.ModelId,
                Temperature = settings.Temperature,
                TopP = settings.TopP,
                PresencePenalty = settings.PresencePenalty,
                FrequencyPenalty = settings.FrequencyPenalty,
                MaxTokens = settings.MaxTokens,
                SystemPrompt = systemPrompt
            };

            SaveMetadata(metadata);

            fSessions[sessionId] = metadata;
            fCurrentHistory = new List<ChatMessage>();
            fCurrentSessionId = sessionId;

            // Add system prompt to the beginning of the history if it's not there yet
            if (fCurrentHistory.Count == 0) {
                fCurrentHistory.Add(new ChatMessage("system", systemPrompt + string.Format("\nsession_id: {0}", fCurrentSessionId)));
            }
        }

        public async Task AddToCurrentHistory(ChatMessage message)
        {
            fCurrentHistory.Add(message);

            await SaveCurrentSessionAsync();
        }

        public void RemoveSession(string sessionId)
        {
            if (string.IsNullOrEmpty(sessionId))
                return;

            if (fSessions.ContainsKey(sessionId)) {
                fSessions.Remove(sessionId);

                var chatPath = Path.Combine(fBasePath, sessionId);
                if (Directory.Exists(chatPath)) {
                    try {
                        Directory.Delete(chatPath, true);
                    } catch {
                        // Ignore deletion errors
                    }
                }
            }
        }

        public void RenameSession(string sessionId, string newTitle)
        {
            if (fSessions.TryGetValue(sessionId, out var metadata)) {
                metadata.Title = newTitle;
                SaveMetadata(metadata);
            }
        }

        private void SaveMetadata(Session metadata)
        {
            var chatPath = Path.Combine(fBasePath, metadata.Id);
            var metadataFile = Path.Combine(chatPath, "metadata.json");

            var options = new JsonSerializerOptions { WriteIndented = true };
            var json = JsonSerializer.Serialize(metadata, options);
            File.WriteAllText(metadataFile, json);
        }
    }
}
