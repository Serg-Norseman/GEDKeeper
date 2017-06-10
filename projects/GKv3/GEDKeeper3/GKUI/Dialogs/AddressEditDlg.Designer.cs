using System;
using Eto.Drawing;
using Eto.Forms;

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
            tabsAddrData.SuspendLayout();
            pageCommon.SuspendLayout();
            SuspendLayout();

            btnAccept.ImagePosition = ButtonImagePosition.Left;
            btnAccept.Location = new Point(325, 340);
            btnAccept.Size = new Size(113, 30);
            btnAccept.Text = "btnAccept";
            btnAccept.Click += btnAccept_Click;

            btnCancel.ImagePosition = ButtonImagePosition.Left;
            btnCancel.Location = new Point(448, 340);
            btnCancel.Size = new Size(113, 30);
            btnCancel.Text = "btnCancel";

            tabsAddrData.Controls.Add(pageCommon);
            tabsAddrData.Controls.Add(pagePhones);
            tabsAddrData.Controls.Add(pageEmails);
            tabsAddrData.Controls.Add(pageWebPages);
            tabsAddrData.Dock = DockStyle.Top;
            tabsAddrData.Location = new Point(0, 0);
            tabsAddrData.SelectedIndex = 0;
            tabsAddrData.Size = new Size(572, 321);

            pageCommon.Controls.Add(lblCountry);
            pageCommon.Controls.Add(lblState);
            pageCommon.Controls.Add(lblCity);
            pageCommon.Controls.Add(lblPostalCode);
            pageCommon.Controls.Add(lblAddress);
            pageCommon.Controls.Add(txtCountry);
            pageCommon.Controls.Add(txtState);
            pageCommon.Controls.Add(txtCity);
            pageCommon.Controls.Add(txtPostalCode);
            pageCommon.Controls.Add(txtAddress);
            pageCommon.Location = new Point(4, 26);
            pageCommon.Size = new Size(564, 291);
            pageCommon.Text = "pageCommon";

            lblCountry.Location = new Point(11, 10);
            lblCountry.Size = new Size(55, 17);
            lblCountry.Text = "lblCountry";

            lblState.Location = new Point(302, 10);
            lblState.Size = new Size(103, 17);
            lblState.Text = "lblState";

            lblCity.Location = new Point(11, 68);
            lblCity.Size = new Size(47, 17);
            lblCity.Text = "lblCity";

            lblPostalCode.Location = new Point(302, 68);
            lblPostalCode.Size = new Size(101, 17);
            lblPostalCode.Text = "lblPostalCode";

            lblAddress.Location = new Point(11, 126);
            lblAddress.Size = new Size(46, 17);
            lblAddress.Text = "lblAddress";

            txtCountry.Location = new Point(11, 29);
            txtCountry.Size = new Size(282, 24);

            txtState.Location = new Point(302, 29);
            txtState.Size = new Size(248, 24);

            txtCity.Location = new Point(11, 87);
            txtCity.Size = new Size(282, 24);

            txtPostalCode.Location = new Point(302, 87);
            txtPostalCode.Size = new Size(248, 24);

            txtAddress.Location = new Point(11, 146);
            txtAddress.Size = new Size(539, 24);

            pagePhones.Location = new Point(4, 26);
            pagePhones.Size = new Size(564, 291);
            pagePhones.Text = "pagePhones";

            pageEmails.Location = new Point(4, 26);
            pageEmails.Size = new Size(564, 291);
            pageEmails.Text = "pageEmails";

            pageWebPages.Location = new Point(4, 26);
            pageWebPages.Size = new Size(564, 291);
            pageWebPages.Text = "pageWebPages";

            DefaultButton = btnAccept;
            AbortButton = btnCancel;
            ClientSize = new Size(572, 385);
            Controls.Add(btnAccept);
            Controls.Add(btnCancel);
            Controls.Add(tabsAddrData);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "AddressEditDlg";
            tabsAddrData.ResumeLayout();
            pageCommon.ResumeLayout();
            ResumeLayout();
        }
    }
}
