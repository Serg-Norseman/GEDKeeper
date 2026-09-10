/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Drawing;
using Eto.Forms;
using GKCore.Locales;

namespace GKMCPPlugin;

public class MCPServerForm : Dialog
{
    private readonly ILangMan fLangMan;
    private readonly Plugin fPlugin;

    public MCPServerForm(Plugin plugin, ILangMan langMan)
    {
        fPlugin = plugin;
        fLangMan = langMan;

        InitControls();
        LayoutForm();
        UpdateUIState();
    }

    private async void OnStartServerClick(object sender, EventArgs e)
    {
        if (!int.TryParse(_portTextBox.Text, out int port) || port < 1 || port > 65535) {
            MessageBox.Show(this, "Укажите корректный номер порта (1-65535).", "Ошибка валидации", MessageBoxButtons.OK, MessageBoxType.Error);
            return;
        }
        fPlugin.ServerPort = port;

        UpdateUIState();
        _statusLabel.Text = "Запуск сервера...";
        _statusLabel.TextColor = Colors.Orange;

        try {
            fPlugin.ServerHost = _hostTextBox.Text;
            fPlugin.EnableCors = _corsCheckBox.Checked ?? false;
            fPlugin.AllowedHosts = _allowedHostsTextBox.Text;
            fPlugin.VerboseLogging = _verboseLoggingCheckBox.Checked ?? false;

            await fPlugin.StartAsync();

            _statusLabel.Text = $"Активен: http://{fPlugin.ServerHost}:{fPlugin.ServerPort}/mcp";
            _statusLabel.TextColor = Colors.Green;
        } catch (Exception ex) {
            _statusLabel.Text = "Ошибка при запуске";
            _statusLabel.TextColor = Colors.Red;
            MessageBox.Show(this, $"Не удалось запустить сервер: {ex.Message}", "Ошибка сети", MessageBoxType.Error);
            UpdateUIState();
        }
    }

    private async void OnStopServerClick(object sender, EventArgs e)
    {
        _statusLabel.Text = "Остановка сервера...";
        _statusLabel.TextColor = Colors.Orange;
        _stopButton.Enabled = false;

        try {
            await fPlugin.StopAsync();

            _statusLabel.Text = "Сервер остановлен";
            _statusLabel.TextColor = SystemColors.ControlText;
        } catch (Exception ex) {
            MessageBox.Show(this, $"Ошибка при остановке сервера: {ex.Message}", "Ошибка", MessageBoxType.Error);
        } finally {
            UpdateUIState();
        }
    }

    #region Design

    private TextBox _hostTextBox;
    private TextBox _portTextBox;
    private TextBox _allowedHostsTextBox;
    private CheckBox _autoStartCheckBox;
    private CheckBox _corsCheckBox;
    private CheckBox _verboseLoggingCheckBox;
    private Button _startButton;
    private Button _stopButton;
    private Label _statusLabel;
    private StackLayout _allowedHostsRow;

    private void InitControls()
    {
        Title = "MCP Server Settings & Control";
        ClientSize = new Size(480, 420);
        MinimumSize = new Size(400, 350);

        _hostTextBox = new TextBox { Text = fPlugin.ServerHost, ToolTip = "IP или хост для прослушивания (например, localhost или 0.0.0.0)" };
        _portTextBox = new TextBox { Text = fPlugin.ServerPort.ToString(), ToolTip = "Порт для соединений" };
        _allowedHostsTextBox = new TextBox { Text = fPlugin.AllowedHosts, ToolTip = "Список разрешенных Origin через запятую" };

        _autoStartCheckBox = new CheckBox { Text = "Автозапуск при старте приложения", Checked = fPlugin.AutoStart };
        _corsCheckBox = new CheckBox { Text = fLangMan.LS(PLS.CORS), Checked = fPlugin.EnableCors };
        _corsCheckBox.CheckedChanged += (s, e) => _allowedHostsRow.Visible = _corsCheckBox.Checked ?? false;

        _verboseLoggingCheckBox = new CheckBox { Text = fLangMan.LS(PLS.VerboseServerLogs), Checked = fPlugin.VerboseLogging };

        _startButton = new Button { Text = fLangMan.LS(PLS.Start), ImagePosition = ButtonImagePosition.Left };
        _startButton.Click += OnStartServerClick;

        _stopButton = new Button { Text = fLangMan.LS(PLS.Stop) };
        _stopButton.Click += OnStopServerClick;

        _statusLabel = new Label { Text = "Сервер остановлен", VerticalAlignment = VerticalAlignment.Center };
    }

    private void LayoutForm()
    {
        var configLayout = new DynamicLayout { Spacing = new Size(10, 10), Padding = new Padding(10) };

        configLayout.AddRow(new Label { Text = fLangMan.LS(PLS.ServerHost), VerticalAlignment = VerticalAlignment.Center }, _hostTextBox);
        configLayout.AddRow(new Label { Text = fLangMan.LS(PLS.ServerPort), VerticalAlignment = VerticalAlignment.Center }, _portTextBox);
        configLayout.AddRow(null, _corsCheckBox);

        _allowedHostsRow = new StackLayout(new Label { Text = fLangMan.LS(PLS.TrustedHosts), VerticalAlignment = VerticalAlignment.Center }, _allowedHostsTextBox) {
            Orientation = Orientation.Horizontal
        };
        configLayout.AddRow(null, _allowedHostsRow);

        configLayout.AddRow(null, _verboseLoggingCheckBox);
        configLayout.AddRow(null, _autoStartCheckBox);

        var controlLayout = new StackLayout {
            Orientation = Orientation.Horizontal,
            Spacing = 10,
            Padding = new Padding(10),
            Items = { _startButton, _stopButton, new StackLayoutItem(_statusLabel, VerticalAlignment.Center, true) }
        };

        var groupBox = new GroupBox { Text = "Конфигурация MCP сервера", Content = configLayout };

        var mainLayout = new DynamicLayout { Padding = new Padding(12) };
        mainLayout.Add(groupBox, yscale: true);
        mainLayout.Add(controlLayout, yscale: false);

        Content = mainLayout;
    }

    private void UpdateUIState()
    {
        var isRunning = fPlugin.IsRunning();

        _startButton.Enabled = !isRunning;
        _stopButton.Enabled = isRunning;

        _hostTextBox.ReadOnly = isRunning;
        _portTextBox.ReadOnly = isRunning;
        _allowedHostsTextBox.ReadOnly = isRunning;
        _corsCheckBox.Enabled = !isRunning;
        _verboseLoggingCheckBox.Enabled = !isRunning;
        _autoStartCheckBox.Enabled = !isRunning;
    }

    #endregion
}
