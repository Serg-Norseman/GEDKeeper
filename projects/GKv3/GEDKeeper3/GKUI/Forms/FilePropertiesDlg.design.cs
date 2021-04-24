﻿using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class FilePropertiesDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private TabPage pageAuthor;
        private Label lblName;
        private Label lblAddress;
        private Label lblTelephone;
        private TextBox txtName;
        private TextBox txtTel;
        private TextArea txtAddress;
        private TabPage pageOther;
        private TabControl tabsData;
        private GKListView lvRecordStats;
        private Button btnLangEdit;
        private TextBox txtLanguage;
        private Label lblLanguage;

        private void InitializeComponent()
        {
            SuspendLayout();

            btnLangEdit = new Button();
            btnLangEdit.Size = new Size(26, 26);
            btnLangEdit.Click += btnLangEdit_Click;

            lblName = new Label();
            lblName.Text = "lblName";

            lblAddress = new Label();
            lblAddress.Text = "lblAddress";

            lblLanguage = new Label();
            lblLanguage.Text = "lblLanguage";

            lblTelephone = new Label();
            lblTelephone.Text = "lblTelephone";

            txtName = new TextBox();

            txtLanguage = new TextBox();
            txtLanguage.ReadOnly = true;

            txtTel = new TextBox();

            txtAddress = new TextArea();

            pageAuthor = new TabPage();
            pageAuthor.Text = "pageAuthor";
            pageAuthor.Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        Cells = { lblName, txtName }
                    },
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { lblAddress, txtAddress }
                    },
                    new TableRow {
                        Cells = { lblTelephone, txtTel }
                    },
                    new TableRow {
                        Cells = {
                            lblLanguage,
                            TableLayout.Horizontal(10, new TableCell(txtLanguage, true), btnLangEdit)
                        }
                    }
                }
            };

            lvRecordStats = new GKListView();
            lvRecordStats.AddColumn("Records", 300);
            lvRecordStats.AddColumn("Count", 100 /*, HorizontalAlignment.Right*/);

            pageOther = new TabPage();
            pageOther.Text = "pageOther";
            pageOther.Content = lvRecordStats;

            tabsData = new TabControl();
            tabsData.Pages.Add(pageAuthor);
            tabsData.Pages.Add(pageOther);
            tabsData.Size = new Size(500, 340);

            btnAccept = new Button();
            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(130, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel = new Button();
            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(130, 26);
            btnCancel.Text = "btnCancel";
            btnCancel.Click += btnCancel_Click;

            Content = new DefTableLayout {
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsData }
                    },
                    UIHelper.MakeDialogFooter(null, btnAccept, btnCancel)
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            Title = "FilePropertiesDlg";

            SetPredefProperties(580, 400);
            ResumeLayout();
        }
    }
}
