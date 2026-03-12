#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class AddressEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private TabView tabsAddrData;
        private TabView.Tab pagePhones;
        private TabView.Tab pageEmails;
        private TabView.Tab pageCommon;
        private TabView.Tab pageWebPages;
        private Label lblCountry;
        private Label lblState;
        private Label lblCity;
        private Label lblPostalCode;
        private Label lblAddress;
        private TextField txtCountry;
        private TextField txtState;
        private TextField txtCity;
        private TextField txtPostalCode;
        private TextView txtAddress;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            tabsAddrData = new TabView();
            pageCommon = new TabView.Tab();
            lblCountry = new Label();
            lblState = new Label();
            lblCity = new Label();
            lblPostalCode = new Label();
            lblAddress = new Label();
            txtCountry = new TextField();
            txtState = new TextField();
            txtCity = new TextField();
            txtPostalCode = new TextField();
            txtAddress = new TextView();
            pagePhones = new TabView.Tab();
            pageEmails = new TabView.Tab();
            pageWebPages = new TabView.Tab();

            tabsAddrData.AddTab(pageCommon, true);
            tabsAddrData.AddTab(pagePhones, false);
            tabsAddrData.AddTab(pageEmails, false);
            tabsAddrData.AddTab(pageWebPages, false);
            tabsAddrData.Location = new Point(0, 0);
            tabsAddrData.Size = new Size(70, 20);
            tabsAddrData.TabIndex = 0;

            var pageComCont = new View();
            pageComCont.Add(lblCountry);
            pageComCont.Add(lblState);
            pageComCont.Add(lblCity);
            pageComCont.Add(lblPostalCode);
            pageComCont.Add(lblAddress);
            pageComCont.Add(txtCountry);
            pageComCont.Add(txtState);
            pageComCont.Add(txtCity);
            pageComCont.Add(txtPostalCode);
            pageComCont.Add(txtAddress);
            pageCommon.View = pageComCont;

            lblCountry.Location = new Point(0, 0);
            lblCountry.TabIndex = 0;

            lblState.Location = new Point(0, 2);
            lblState.TabIndex = 1;

            lblCity.Location = new Point(101, 4);
            lblCity.TabIndex = 2;

            lblPostalCode.Location = new Point(0, 6);
            lblPostalCode.TabIndex = 3;

            lblAddress.Location = new Point(0, 8);
            lblAddress.TabIndex = 4;

            txtCountry.Location = new Point(27, 0);
            txtCountry.Size = new Size(40, 1);
            txtCountry.TabIndex = 0;

            txtState.Location = new Point(27, 2);
            txtState.Size = new Size(40, 1);
            txtState.TabIndex = 1;

            txtCity.Location = new Point(27, 4);
            txtCity.Size = new Size(40, 1);
            txtCity.TabIndex = 2;

            txtPostalCode.Location = new Point(27, 6);
            txtPostalCode.Size = new Size(40, 1);
            txtPostalCode.TabIndex = 3;

            txtAddress.Location = new Point(27, 8);
            txtAddress.Multiline = true;
            txtAddress.Size = new Size(40, 6);
            txtAddress.TabIndex = 4;
            txtAddress.KeyDown += txtAddress_KeyDown;

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 1;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 2;
            btnCancel.Clicked += CancelClickHandler;

            Size = new Size(72, 24);
            Add(tabsAddrData);
            AddButton(btnAccept);
            AddButton(btnCancel);
        }
    }
}
