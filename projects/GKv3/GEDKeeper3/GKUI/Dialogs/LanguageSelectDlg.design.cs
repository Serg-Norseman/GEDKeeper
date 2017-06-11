using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
{
    partial class LanguageSelectDlg
    {
        private ListBox lstLanguages;
        private Button btnCancel;
        private Button btnAccept;

        private void InitializeComponent()
        {
            SuspendLayout();

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(80, 26);
            btnAccept.Text = "OK";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(80, 26);
            btnCancel.Text = "Cancel";

            lstLanguages = new ListBox();

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = {
                            lstLanguages,
                            new TableLayout {
                                Padding = new Padding(10),
                                Spacing = new Size(10, 10),
                                Rows = {
                                    btnAccept, btnCancel, null
                                }
                            }
                        }
                    }
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(446, 409);
            Title = "Select language";
            Load += LanguageSelectDlg_Load;

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
