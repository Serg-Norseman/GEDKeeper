using System;
using Eto.Forms;
using Eto.Drawing;
using GKUI.Dialogs;

namespace GEDKeeper3
{
    partial class MainForm : Form
    {
        void InitializeComponent()
        {
            Title = "My Eto Form";
            ClientSize = new Size(400, 350);
            Icon = Icon.FromResource("Resources.icon_gedkeeper.ico");

            Content = new StackLayout {
                Padding = 10,
                Items = {
                    new Label {
                        Text = "Hello World! ***",
                        Font = new Font(SystemFont.Default, 14)
                    },
                    new Label {
                        Text = "Test",
                        Font = new Font(FontFamilies.Serif, 20)
                    }
                }
            };

            var clickMe = new Command {
                MenuText = "Click Me!",
                ToolBarText = "Click Me!"
            };
            clickMe.Executed += (sender, e) => MessageBox.Show(this, "I was clicked!");

            var quitCommand = new Command {
                MenuText = "Quit",
                Shortcut = Application.Instance.CommonModifier | Keys.Q
            };
            quitCommand.Executed += (sender, e) => Application.Instance.Quit();

            var aboutCommand = new Command { MenuText = "About..." };
            aboutCommand.Executed += (sender, e) => {
                using (var dlg = new AboutDlg()) {
                    dlg.ShowModal();
                }
            };

            Menu = new MenuBar {
                Items = {
                    new ButtonMenuItem { Text = "&File", Items = { clickMe } },
                    // new ButtonMenuItem { Text = "&Edit", Items = { /* commands/items */ } },
                    // new ButtonMenuItem { Text = "&View", Items = { /* commands/items */ } },
                },
                ApplicationItems = {
                    // application (OS X) or file menu (others)
                    new ButtonMenuItem { Text = "&Preferences..." },
                },
                QuitItem = quitCommand,
                AboutItem = aboutCommand
            };

            ToolBar = new ToolBar { Items = { clickMe } };
        }
    }
}