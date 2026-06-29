/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Threading.Tasks;
using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKCore.Locales;
using GKCortex.Features;
using GKCortex.LMChat;
using GKCortex.Utilities;

namespace GKLMChatPlugin
{
    public class LMChatForm : Form, ILMChatView
    {
        private readonly ILangMan fLangMan;
        private readonly LMChatClient fLMClient;
        private readonly LMSettings fLMSettings;


        public LMChatForm(Plugin plugin)
        {
            fLangMan = plugin.LangMan;
            fLMSettings = plugin.LMSettings;

            fLMClient = new LMChatClient(fLMSettings);
            fLMClient.View = this;
            fLMClient.HistoryStorage.BasePath = Path.Combine(AppHost.Instance.GetAppDataPath(), "lmchat");

            MCPController.SetLMChat(fLMClient);

            InitLayout();
            InitChatWebpage();
            LoadModelsAsync().ConfigureAwait(false);
            LoadSessionsAsync();
        }

        #region Design

        private WebView fWebView;
        private TextArea fInputArea;
        private Button fSendButton;
        private Button fStopButton;
        private DropDown fModelDropDown;
        private DropDown fSessionDropDown;

        private void InitLayout()
        {
            Title = fLangMan.LS(PLS.Title);
            ClientSize = new Size(800, 600);
            MinimumSize = new Size(500, 400);

            fWebView = new WebView();
            fInputArea = new TextArea { Wrap = true };
            fSendButton = new Button { Text = fLangMan.LS(PLS.Send) };
            fStopButton = new Button { Text = fLangMan.LS(PLS.Stop), Enabled = false };
            fModelDropDown = new DropDown();
            fSessionDropDown = new DropDown();
            var renameSessionButton = new ButtonMenuItem { Text = fLangMan.LS(PLS.RenameSession) };
            var settingsButton = new Button { Text = "⚙ " + fLangMan.LS(PLS.Settings) };
            var newSessionButton = new ButtonMenuItem { Text = fLangMan.LS(PLS.NewSession) };
            var sessionActionsButton = new Button { Text = "✎ " + fLangMan.LS(PLS.Session) };

            var topPanel = new StackLayout {
                Orientation = Orientation.Horizontal,
                Spacing = 10,
                Padding = new Padding(10, 5),
                Items = {
                    settingsButton,
                    new Label { Text = fLangMan.LS(PLS.Model) },
                    fModelDropDown,
                    new Label { Text = fLangMan.LS(PLS.Session) },
                    fSessionDropDown,
                    sessionActionsButton
                }
            };

            fInputArea.Height = 80;
            var bottomGrid = new StackLayout {
                Orientation = Orientation.Horizontal,
                Spacing = 4,
                Padding = new Padding(4, 4),
                Items = {
                    new StackLayoutItem(fInputArea, true),
                    new StackLayout { Orientation = Orientation.Vertical, Spacing = 4, Items = { fSendButton, fStopButton } }
                }
            };

            Content = new TableLayout {
                Rows = { topPanel, new TableRow(fWebView) { ScaleHeight = true }, bottomGrid }
            };

            fSendButton.Click += async (s, e) => await SendMessageAsync();
            fStopButton.Click += (s, e) => StopMessage();
            settingsButton.Click += (s, e) => ShowSettingsDialog();
            newSessionButton.Click += (s, e) => NewSession();
            renameSessionButton.Click += (s, e) => RenameSession();
            fSessionDropDown.SelectedIndexChanged += (s, e) => LoadSelectedSession();

            sessionActionsButton.ContextMenu = new ContextMenu {
                Items = {
                    renameSessionButton,
                    newSessionButton,
                }
            };
            sessionActionsButton.Click += (s, e) => {
                var ctxMenu = (s as Button).ContextMenu;
                var buttonRect = (s as Button).Bounds;
                ctxMenu.Show(sessionActionsButton, buttonRect.BottomLeft);
            };

            fModelDropDown.SelectedIndexChanged += (s, e) => {
                object selectedModel = fModelDropDown.SelectedValue;
                fLMSettings.ModelId = selectedModel.ToString();
            };
        }

        private async Task RenameSession()
        {
            if (fSessionDropDown.SelectedKey == null) return;

            var sessionId = fSessionDropDown.SelectedKey.ToString();
            var newName = await AppHost.StdDialogs.GetInput(this, fLangMan.LS(PLS.RenameSession), "");

            if (!string.IsNullOrEmpty(newName)) {
                fLMClient.HistoryStorage.RenameSession(sessionId, newName);
                LoadSessionsAsync();
            }
        }

        private async Task LoadSelectedSession()
        {
            if (fSessionDropDown.SelectedKey == null) return;

            var sessionId = fSessionDropDown.SelectedKey.ToString();
            await fLMClient.HistoryStorage.LoadSessionAsync(sessionId);

            await fWebView.ExecuteScriptAsync("document.getElementById('chat').innerHTML = '';");
            foreach (var msg in fLMClient.HistoryStorage.CurrentHistory) {
                ShowMessage(msg.Content, msg.Role);
            }
        }

        private async void LoadSessionsAsync()
        {
            try {
                var sessions = await Task.Run(() => fLMClient.HistoryStorage.Sessions.Values
                    .OrderByDescending(s => s.StartDate)
                    .ToList());

                Application.Instance.AsyncInvoke(() => {
                    fSessionDropDown.Items.Clear();
                    foreach (var session in sessions) {
                        fSessionDropDown.Items.Add(session.Title, session.Id);
                    }
                    if (fSessionDropDown.Items.Count > 0)
                        fSessionDropDown.SelectedIndex = 0;
                });
            } catch (Exception ex) {
                Application.Instance.AsyncInvoke(() => MessageBox.Show(this, $"Error loading sessions: {ex.Message}"));
            }
        }

        /// <summary>
        /// Start a new session.
        /// </summary>
        private void NewSession()
        {
            fLMClient.NewSession();
            LoadSessionsAsync();

            // Clear the web view chat display
            fWebView.ExecuteScriptAsync("document.getElementById('chat').innerHTML = '';");
        }

        /// <summary>
        /// Stop the current request.
        /// </summary>
        private void StopMessage()
        {
            fLMClient.CancelRequest();
            fStopButton.Enabled = false;
            fSendButton.Enabled = true;
        }

        private void ShowSettingsDialog()
        {
            var dialog = new Dialog();
            dialog.Title = fLangMan.LS(PLS.Settings);
            dialog.ClientSize = new Size(400, 400);

            // Create controls for parameters
            var apiAddressLabel = new Label { Text = fLangMan.LS(PLS.APIAddress) };
            var apiAddressTextBox = new TextBox { Text = fLMSettings.APIAddress };

            var apiKeyLabel = new Label { Text = fLangMan.LS(PLS.APIKey) };
            var apiKeyTextBox = new TextBox { Text = fLMSettings.APIKey };

            var temperatureLabel = new Label { Text = fLangMan.LS(PLS.Temperature) };
            var temperatureSlider = new Slider { MinValue = 0, MaxValue = 100, Value = (int)(fLMSettings.Temperature * 100) };
            var temperatureValueLabel = new Label { Text = fLMSettings.Temperature.ToString("F2") };

            var topPLabel = new Label { Text = fLangMan.LS(PLS.TopP) };
            var topPSlider = new Slider { MinValue = 0, MaxValue = 100, Value = (int)(fLMSettings.TopP * 100) };
            var topPValueLabel = new Label { Text = fLMSettings.TopP.ToString("F2") };

            var presenceLabel = new Label { Text = fLangMan.LS(PLS.PresencePenalty) };
            var presenceSlider = new Slider { MinValue = -200, MaxValue = 200, Value = (int)(fLMSettings.PresencePenalty * 100) };
            var presenceValueLabel = new Label { Text = fLMSettings.PresencePenalty.ToString("F2") };

            var frequencyLabel = new Label { Text = fLangMan.LS(PLS.FrequencyPenalty) };
            var frequencySlider = new Slider { MinValue = -200, MaxValue = 200, Value = (int)(fLMSettings.FrequencyPenalty * 100) };
            var frequencyValueLabel = new Label { Text = fLMSettings.FrequencyPenalty.ToString("F2") };

            var maxTokensLabel = new Label { Text = fLangMan.LS(PLS.MaxTokens) };
            var maxTokensNumeric = new NumericStepper { MinValue = 1, MaxValue = 8192, Value = fLMSettings.MaxTokens };

            var streamModeCheckBox = new CheckBox { Text = fLangMan.LS(PLS.StreamMode), Checked = fLMSettings.StreamMode };

            var systemPromptLabel = new Label { Text = fLangMan.LS(PLS.SystemPrompt) };
            var systemPromptTextArea = new TextArea {
                Text = fLMClient.SystemPrompt,
                Height = 100,
                Wrap = true
            };

            // Event handlers for updating values
            apiAddressTextBox.TextChanged += (s, e) => {
                fLMSettings.APIAddress = apiAddressTextBox.Text;
            };

            apiKeyTextBox.TextChanged += (s, e) => {
                fLMSettings.APIKey = apiKeyTextBox.Text;
            };

            temperatureSlider.ValueChanged += (s, e) => {
                fLMSettings.Temperature = temperatureSlider.Value / 100.0;
                temperatureValueLabel.Text = fLMSettings.Temperature.ToString("F2");
            };

            topPSlider.ValueChanged += (s, e) => {
                fLMSettings.TopP = topPSlider.Value / 100.0;
                topPValueLabel.Text = fLMSettings.TopP.ToString("F2");
            };

            presenceSlider.ValueChanged += (s, e) => {
                fLMSettings.PresencePenalty = presenceSlider.Value / 100.0;
                presenceValueLabel.Text = fLMSettings.PresencePenalty.ToString("F2");
            };

            frequencySlider.ValueChanged += (s, e) => {
                fLMSettings.FrequencyPenalty = frequencySlider.Value / 100.0;
                frequencyValueLabel.Text = fLMSettings.FrequencyPenalty.ToString("F2");
            };

            maxTokensNumeric.ValueChanged += (s, e) => {
                fLMSettings.MaxTokens = (int)maxTokensNumeric.Value;
            };

            streamModeCheckBox.CheckedChanged += (s, e) => {
                fLMSettings.StreamMode = streamModeCheckBox.Checked ?? false;
            };

            systemPromptTextArea.TextChanged += (s, e) => {
                fLMClient.SystemPrompt = systemPromptTextArea.Text;
            };

            var okButton = new Button { Text = "OK" };
            okButton.Click += async (s, e) => {
                dialog.Close();
                await LoadModelsAsync();
            };

            var tableLayout = new TableLayout {
                Spacing = new Size(5, 5),
                Padding = new Padding(10),
                Rows = {
                    new TableLayout {
                        Spacing = new Size(5, 5),
                        Rows = {
                            new TableRow(apiAddressLabel, apiAddressTextBox),
                            new TableRow(apiKeyLabel, apiKeyTextBox),
                        }
                    },
                    new TableLayout {
                        Spacing = new Size(5, 5),
                        Rows = {
                            new TableRow(temperatureLabel, temperatureSlider, temperatureValueLabel),
                            new TableRow(topPLabel, topPSlider, topPValueLabel),
                            new TableRow(presenceLabel, presenceSlider, presenceValueLabel),
                            new TableRow(frequencyLabel, frequencySlider, frequencyValueLabel),
                            new TableRow(maxTokensLabel, maxTokensNumeric, null),
                        }
                    },
                    new TableRow(streamModeCheckBox),
                    new TableRow(systemPromptLabel),
                    new TableRow(systemPromptTextArea) { ScaleHeight = true },
                    new StackLayout { Orientation = Orientation.Horizontal, Items = { new StackLayoutItem(null, true), okButton } }
                }
            };

            dialog.Content = tableLayout;
            dialog.ShowModal(this);
        }

        private void InitChatWebpage()
        {
            string baseHtml = @"
            <!DOCTYPE html>
            <html>
            <head>
                <script>
                </script>
                <style>
                    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; margin: 0; padding: 15px; background: #f9f9f9; color: #333; }
                    .chat-container { display: flex; flex-direction: column; gap: 12px; width: 100%; }
                    .chat-container > * + * { margin-top: 12px; }
                    .chat-container::after { content: ''; display: table; clear: both; }
                    .msg { padding: 10px 14px; border-radius: 8px; max-width: 85%; line-height: 1.5; word-wrap: break-word; box-sizing: border-box; }
                    .user { background: #00a8f4; color: white; align-self: flex-end; }
                    .assistant { background: #eaeaea; color: #111; align-self: flex-start; box-shadow: 0 1px 2px rgba(0,0,0,0.1); }
                    .system { background: #f0e1fc; color: #111; align-self: flex-start; }
                    .tool { background: #ffecb8; color: #111; align-self: flex-start; }
                    .reasoning { font-style: italic; color: #666; background: #f0f0f0; border-left: 3px solid #999; padding: 5px 10px; margin-bottom: 8px; font-size: 0.9em; }
                    table { border-collapse: collapse; margin: 10px 0; width: 100%; font-size: 0.95em; }
                    th, td { border: 1px solid #ccc; padding: 6px 10px; text-align: left; }
                    th { background-color: #ddd; }
                    p { margin: 4px 0; }
                    .streaming { background: #fff9c4; color: #333; }
                </style>
            </head>
            <body>
                <div class='chat-container' id='chat'></div>
            </body>
            </html>";

            fWebView.LoadHtml(baseHtml);
        }

        #endregion

        public void ShowMessage(string msg, string role)
        {
            if (string.IsNullOrEmpty(msg) || msg == "\"\"") return;

            Application.Instance.AsyncInvoke(() => {
                string processedMsg = CLIHelper.ConvertMarkdownToHtml(msg);
                // Escape content for JavaScript
                string escapedContent = processedMsg.Replace("'", "\\'").Replace("\n", "\\n").Replace("\r", "\\r");

                fWebView.ExecuteScriptAsync($@"document.getElementById('chat').insertAdjacentHTML('beforeend', '<div class=""msg {role}"">{escapedContent}</div>'); window.scrollTo(0, document.body.scrollHeight);");
            });
        }

        void ILMChatView.StartStreamingMessage(int requestId)
        {
            Application.Instance.AsyncInvoke(() => {
                fWebView.ExecuteScriptAsync($@"document.getElementById('chat').insertAdjacentHTML('beforeend', '<div id=""msg_{requestId}"" class=""msg assistant streaming"">...</div>'); window.scrollTo(0, document.body.scrollHeight);");
            });
        }

        void ILMChatView.UpdateStreamingMessage(int requestId, string content)
        {
            Application.Instance.AsyncInvoke(() => {
                string processedMsg = CLIHelper.ConvertMarkdownToHtml(content);
                // Escape content for JavaScript
                string escapedContent = processedMsg.Replace("'", "\\'").Replace("\n", "\\n").Replace("\r", "\\r");

                fWebView.ExecuteScriptAsync($@"document.getElementById('msg_{requestId}').innerHTML = '{escapedContent}'; window.scrollTo(0, document.body.scrollHeight);");
            });
        }

        void ILMChatView.FinalizeStreamingMessage(int requestId)
        {
            Application.Instance.AsyncInvoke(() => {
                fWebView.ExecuteScriptAsync($@"document.getElementById('msg_{requestId}').className = 'msg assistant';");
            });
        }

        private async Task LoadModelsAsync()
        {
            try {
                var models = await fLMClient.LoadModelsAsync();

                Application.Instance.AsyncInvoke(() => {
                    fModelDropDown.Items.Clear();
                    foreach (var m in models) {
                        fModelDropDown.Items.Add(m.Id);
                    }
                    if (fModelDropDown.Items.Count > 0) fModelDropDown.SelectedIndex = 0;
                });
            } catch (Exception ex) {
                Application.Instance.AsyncInvoke(() => MessageBox.Show(this, $"Error loading models: {ex.Message}"));
            }
        }

        private async Task SendMessageAsync()
        {
            string userText = fInputArea.Text?.Trim();
            if (string.IsNullOrEmpty(userText) || string.IsNullOrEmpty(fLMSettings.ModelId)) return;

            fInputArea.Text = string.Empty;
            fSendButton.Enabled = false;
            fStopButton.Enabled = true;

            await fLMClient.AddHistory("user", userText);
            ShowMessage(userText, "user");

            try {
                await fLMClient.SendMessageAsync();
            } catch (Exception ex) {
                ShowMessage($"Error: {JsonSerializer.Serialize(ex.Message)}", "assistant");
            } finally {
                Application.Instance.AsyncInvoke(() => {
                    fSendButton.Enabled = true;
                    fStopButton.Enabled = false;
                });
            }
        }
    }
}
