/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Eto.Drawing;
using Eto.Forms;
using GKCore.Locales;
using GKCortex.LMChat;
using GKCortex.MCP;
using GKCortex.Utilities;

namespace GKLMChatPlugin
{
    public class LMChatForm : Form
    {
        private const string APIUrl = "http://127.0.0.1:1337/";

        private readonly ILangMan fLangMan;
        private readonly LMChatClient fLMClient;
        private readonly CancellationTokenSource fTokenSource;

        public LMChatForm(ILangMan langMan, MCPServer mcpServer)
        {
            fLangMan = langMan;
            fLMClient = new LMChatClient(APIUrl, "", null, mcpServer);
            fTokenSource = new CancellationTokenSource(TimeSpan.FromMinutes(5));

            InitLayout();
            InitChatWebpage();
            LoadModelsAsync().ConfigureAwait(false);
        }

        #region Design

        private WebView _webView;
        private TextArea _inputArea;
        private Button _sendButton;
        private DropDown _modelDropDown;
        private Label _loadingLabel;
        private Button _settingsButton;

        private void InitLayout()
        {
            Title = "GEDKeeper AI Assistant";
            ClientSize = new Size(800, 600);
            MinimumSize = new Size(500, 400);

            _webView = new WebView();
            _inputArea = new TextArea { Wrap = true };
            _sendButton = new Button { Text = "Send" };
            _modelDropDown = new DropDown();
            _loadingLabel = new Label { Text = "Loading...", Visible = false };
            _settingsButton = new Button { Text = "Settings" };

            var topPanel = new StackLayout {
                Orientation = Orientation.Horizontal,
                Spacing = 10,
                Padding = new Padding(10, 5),
                Items = { new Label { Text = "Model:" }, _modelDropDown, _loadingLabel, _settingsButton }
            };

            _inputArea.Height = 60;
            _inputArea.Width = 700;
            var bottomGrid = new DynamicLayout { Padding = new Padding(10) };
            bottomGrid.AddRow(_inputArea, _sendButton);

            Content = new TableLayout {
                Rows = { topPanel, new TableRow(_webView) { ScaleHeight = true }, bottomGrid }
            };

            _sendButton.Click += async (s, e) => await SendMessageAsync();
            _settingsButton.Click += (s, e) => ShowSettingsDialog();
        }

        private void ShowSettingsDialog()
        {
            var dialog = new Dialog();
            dialog.Title = "Model Settings";
            dialog.ClientSize = new Size(400, 400);

            // Create controls for parameters
            var temperatureLabel = new Label { Text = "Temperature:" };
            var temperatureSlider = new Slider { MinValue = 0, MaxValue = 100, Value = (int)(fLMClient.Temperature * 100) };
            var temperatureValueLabel = new Label { Text = fLMClient.Temperature.ToString("F2") };

            var topPLabel = new Label { Text = "Top-P:" };
            var topPSlider = new Slider { MinValue = 0, MaxValue = 100, Value = (int)(fLMClient.TopP * 100) };
            var topPValueLabel = new Label { Text = fLMClient.TopP.ToString("F2") };

            var presenceLabel = new Label { Text = "Presence Penalty:" };
            var presenceSlider = new Slider { MinValue = -200, MaxValue = 200, Value = (int)(fLMClient.PresencePenalty * 100) };
            var presenceValueLabel = new Label { Text = fLMClient.PresencePenalty.ToString("F2") };

            var frequencyLabel = new Label { Text = "Frequency Penalty:" };
            var frequencySlider = new Slider { MinValue = -200, MaxValue = 200, Value = (int)(fLMClient.FrequencyPenalty * 100) };
            var frequencyValueLabel = new Label { Text = fLMClient.FrequencyPenalty.ToString("F2") };

            var maxTokensLabel = new Label { Text = "Max Tokens:" };
            var maxTokensNumeric = new NumericUpDown { MinValue = 1, MaxValue = 8192, Value = fLMClient.MaxTokens };

            var reasoningCheckBox = new CheckBox { Text = "Reasoning Mode", Checked = fLMClient.ReasoningMode, Enabled = false };

            // System prompt field
            var systemPromptLabel = new Label { Text = "System Prompt:" };
            var systemPromptTextArea = new TextArea {
                Text = fLMClient.SystemPrompt,
                Height = 100,
                Wrap = true
            };

            // Event handlers for updating values
            temperatureSlider.ValueChanged += (s, e) => {
                fLMClient.Temperature = temperatureSlider.Value / 100.0;
                temperatureValueLabel.Text = fLMClient.Temperature.ToString("F2");
            };

            topPSlider.ValueChanged += (s, e) => {
                fLMClient.TopP = topPSlider.Value / 100.0;
                topPValueLabel.Text = fLMClient.TopP.ToString("F2");
            };

            presenceSlider.ValueChanged += (s, e) => {
                fLMClient.PresencePenalty = presenceSlider.Value / 100.0;
                presenceValueLabel.Text = fLMClient.PresencePenalty.ToString("F2");
            };

            frequencySlider.ValueChanged += (s, e) => {
                fLMClient.FrequencyPenalty = frequencySlider.Value / 100.0;
                frequencyValueLabel.Text = fLMClient.FrequencyPenalty.ToString("F2");
            };

            maxTokensNumeric.ValueChanged += (s, e) => {
                fLMClient.MaxTokens = (int)maxTokensNumeric.Value;
            };

            reasoningCheckBox.CheckedChanged += (s, e) => {
                fLMClient.ReasoningMode = reasoningCheckBox.Checked ?? false;
            };

            // System prompt handler
            systemPromptTextArea.TextChanged += (s, e) => {
                fLMClient.SystemPrompt = systemPromptTextArea.Text;
            };

            var okButton = new Button { Text = "OK" };
            okButton.Click += (s, e) => dialog.Close();

            var tableLayout = new TableLayout {
                Spacing = new Size(5, 5),
                Padding = new Padding(10),
                Rows = {
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
                    new TableRow(reasoningCheckBox),
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
                    .chat-container { display: flex; flex-direction: column; gap: 12px; }
                    .msg { padding: 10px 14px; border-radius: 8px; max-width: 85%; line-height: 1.5; word-wrap: break-word; }
                    .user { background: #0078d4; color: white; align-self: flex-end; }
                    .assistant { background: #eaeaea; color: #111; align-self: flex-start; box-shadow: 0 1px 2px rgba(0,0,0,0.1); }
                    .reasoning { font-style: italic; color: #666; background: #f0f0f0; border-left: 3px solid #999; padding: 5px 10px; margin-bottom: 8px; font-size: 0.9em; }
                    table { border-collapse: collapse; margin: 10px 0; width: 100%; font-size: 0.95em; }
                    th, td { border: 1px solid #ccc; padding: 6px 10px; text-align: left; }
                    th { background-color: #ddd; }
                    p { margin: 4px 0; }
                </style>
            </head>
            <body>
                <div class='chat-container' id='chat'></div>
            </body>
            </html>";

            //<script src='https://jsdelivr.net'></script>
            //div.innerHTML = markdig.Markdig.toHtml(htmlContent);

            _webView.LoadHtml(baseHtml);
        }

        private void ExecuteJs(string msg, string role)
        {
            // Convert markdown tables to HTML tables
            string processedMsg = CLIHelper.ConvertMarkdownTablesToHtml(msg);

            _webView.ExecuteScriptAsync($@"document.getElementById('chat').insertAdjacentHTML('beforeend', '<div class=""msg {role}"">{processedMsg}</div>'); window.scrollTo(0, document.body.scrollHeight);");
        }

        #endregion

        private async Task LoadModelsAsync()
        {
            try {
                var models = await fLMClient.LoadModelsAsync();

                Application.Instance.AsyncInvoke(() => {
                    _modelDropDown.Items.Clear();
                    foreach (var m in models) {
                        _modelDropDown.Items.Add(m.Id);
                    }
                    if (_modelDropDown.Items.Count > 0) _modelDropDown.SelectedIndex = 0;
                });
            } catch (Exception ex) {
                Application.Instance.AsyncInvoke(() => MessageBox.Show(this, $"Error loading models: {ex.Message}"));
            }
        }

        private async Task SendMessageAsync()
        {
            string userText = _inputArea.Text?.Trim();
            object selectedModel = _modelDropDown.SelectedValue;

            if (string.IsNullOrEmpty(userText) || selectedModel == null) return;

            _inputArea.Text = string.Empty;
            _sendButton.Enabled = false;
            _loadingLabel.Visible = true;

            fLMClient.ModelId = selectedModel.ToString();
            fLMClient.AddHistory("user", userText);
            ExecuteJs(userText, "user");

            try {
                var response = await fLMClient.SendMessageAsync(fTokenSource.Token);
                if (!string.IsNullOrEmpty(response)) {
                    fLMClient.AddHistory("assistant", response);

                    Application.Instance.AsyncInvoke(() => {
                        ExecuteJs(JsonSerializer.Serialize(response), "assistant");
                    });
                }
            } catch (Exception ex) {
                ExecuteJs($"Error: {JsonSerializer.Serialize(ex.Message)}", "assistant");
            } finally {
                Application.Instance.AsyncInvoke(() => {
                    _sendButton.Enabled = true;
                    _loadingLabel.Visible = false;
                });
            }
        }
    }
}
