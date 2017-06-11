using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI.Dialogs
{
    partial class AddressEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private TabControl tabsAddrData;
        private TabPage pagePhones;
        private TabPage pageEmails;
        private TabPage pageCommon;
        private TabPage pageWebPages;
        private Label lblCountry;
        private Label lblState;
        private Label lblCity;
        private Label lblPostalCode;
        private Label lblAddress;
        private TextBox txtCountry;
        private TextBox txtState;
        private TextBox txtCity;
        private TextBox txtPostalCode;
        private TextBox txtAddress;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            tabsAddrData = new TabControl();
            pageCommon = new TabPage();
            lblCountry = new Label();
            lblState = new Label();
            lblCity = new Label();
            lblPostalCode = new Label();
            lblAddress = new Label();
            txtCountry = new TextBox();
            txtState = new TextBox();
            txtCity = new TextBox();
            txtPostalCode = new TextBox();
            txtAddress = new TextBox();
            pagePhones = new TabPage();
            pageEmails = new TabPage();
            pageWebPages = new TabPage();

            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Size = new Size(80, 26);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Size = new Size(80, 26);
            btnCancel.Text = "btnCancel";

            tabsAddrData.Pages.Add(pageCommon);
            tabsAddrData.Pages.Add(pagePhones);
            tabsAddrData.Pages.Add(pageEmails);
            tabsAddrData.Pages.Add(pageWebPages);
            tabsAddrData.SelectedIndex = 0;
            tabsAddrData.Size = new Size(572, 321);

            pageCommon.Size = new Size(564, 291);
            pageCommon.Text = "pageCommon";
            pageCommon.Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        Cells = { lblCountry, lblState }
                    },
                    new TableRow {
                        Cells = { txtCountry, txtState }
                    },
                    new TableRow {
                        Cells = { lblCity, lblPostalCode }
                    },
                    new TableRow {
                        Cells = { txtCity, txtPostalCode }
                    },
                    new TableRow {
                        Cells = { lblAddress }
                    },
                    new TableRow {
                        Cells = { txtAddress }
                    },
                    null
                }
            };

            lblCountry.Size = new Size(55, 17);
            lblCountry.Text = "lblCountry";

            lblState.Size = new Size(103, 17);
            lblState.Text = "lblState";

            lblCity.Size = new Size(47, 17);
            lblCity.Text = "lblCity";

            lblPostalCode.Size = new Size(101, 17);
            lblPostalCode.Text = "lblPostalCode";

            lblAddress.Size = new Size(46, 17);
            lblAddress.Text = "lblAddress";

            txtCountry.Size = new Size(282, 24);

            txtState.Size = new Size(248, 24);

            txtCity.Size = new Size(282, 24);

            txtPostalCode.Size = new Size(248, 24);

            txtAddress.Size = new Size(539, 24);

            pagePhones.Size = new Size(564, 291);
            pagePhones.Text = "pagePhones";

            pageEmails.Size = new Size(564, 291);
            pageEmails.Text = "pageEmails";

            pageWebPages.Size = new Size(564, 291);
            pageWebPages.Text = "pageWebPages";

            Content = new TableLayout {
                Padding = new Padding(10),
                Spacing = new Size(10, 10),
                Rows = {
                    new TableRow {
                        ScaleHeight = true,
                        Cells = { tabsAddrData }
                    },
                    new TableRow {
                        ScaleHeight = false,
                        Cells = { null, btnAccept, btnCancel }
                    }
                }
            };

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(572, 385);
            Title = "AddressEditDlg";

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
