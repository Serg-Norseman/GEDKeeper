using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
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
        private TabControl PageControl1;
        private GKListViewStub lvRecordStats;
        private Button btnLangEdit;
        private TextBox txtLanguage;
        private Label lblLanguage;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            PageControl1 = new TabControl();
            pageAuthor = new TabPage();
            btnLangEdit = new Button();
            lblName = new Label();
            lblAddress = new Label();
            lblLanguage = new Label();
            lblTelephone = new Label();
            txtName = new TextBox();
            txtLanguage = new TextBox();
            txtTel = new TextBox();
            txtAddress = new TextArea();
            pageOther = new TabPage();
            lvRecordStats = new GKListViewStub();

            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(112, 31);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(112, 31);
            btnCancel.Text = "btnCancel";

            PageControl1.Pages.Add(pageAuthor);
            PageControl1.Pages.Add(pageOther);
            PageControl1.SelectedIndex = 0;
            PageControl1.Size = new Size(606, 331);

            pageAuthor.Size = new Size(598, 301);
            pageAuthor.Text = "pageAuthor";
            pageAuthor.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
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
                        Cells = { lblLanguage, txtLanguage, btnLangEdit }
                    }
                }
            };

            btnLangEdit.Size = new Size(39, 34);
            btnLangEdit.Click += btnLangEdit_Click;

            lblName.Size = new Size(55, 17);
            lblName.Text = "lblName";

            lblAddress.Size = new Size(68, 17);
            lblAddress.Text = "lblAddress";

            lblLanguage.Size = new Size(80, 17);
            lblLanguage.Text = "lblLanguage";

            lblTelephone.Size = new Size(83, 17);
            lblTelephone.Text = "lblTelephone";

            txtName.Size = new Size(439, 24);

            txtLanguage.ReadOnly = true;
            txtLanguage.Size = new Size(386, 24);

            txtTel.Size = new Size(439, 24);

            txtAddress.Size = new Size(439, 136);

            pageOther.Size = new Size(598, 301);
            pageOther.Text = "pageOther";
            pageOther.Content = lvRecordStats;

            //lvRecordStats.FullRowSelect = true;
            //lvRecordStats.MultiSelect = false;
            lvRecordStats.Size = new Size(598, 301);
            //lvRecordStats.View = View.Details;

            /*columnHeader1.Text = "Records";
            columnHeader1.Width = 300;

            columnHeader2.Text = "Count";
            columnHeader2.TextAlign = HorizontalAlignment.Right;
            columnHeader2.Width = 100;*/

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { PageControl1 }
                    },
                    new TableRow {
                        ScaleHeight = false,
                        Cells = { null, btnAccept, btnCancel }
                    }
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(630, 405);
            Title = "FilePropertiesDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
