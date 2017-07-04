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
            btnAccept.Size = new Size(130, 26);
            btnAccept.Text = "OK";
            btnAccept.Click += btnAccept_Click;
            btnAccept.Image = Bitmap.FromResource("Resources.btn_accept.gif");

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(130, 26);
            btnCancel.Text = "Cancel";
            btnCancel.Click += CancelClickHandler;
            btnCancel.Image = Bitmap.FromResource("Resources.btn_cancel.gif");

            lstLanguages = new ListBox();

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = {
                            new TableCell(lstLanguages, true),
                            new TableLayout {
                                Padding = new Padding(0),
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
            Title = "Select language";
            Load += LanguageSelectDlg_Load;

            SetPredefProperties(440, 400);
            ResumeLayout();
        }
    }
}
