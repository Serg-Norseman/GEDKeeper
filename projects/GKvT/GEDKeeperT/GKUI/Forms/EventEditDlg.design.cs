#pragma warning disable IDE1006 // Naming Styles

using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class EventEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private TabView tabsData;
        private TabView.Tab pageNotes;
        private TabView.Tab pageMultimedia;
        private TabView.Tab pageSources;
        private Button btnAddress;
        private TabView.Tab pageCommon;
        private Label lblEvent;
        private Label lblPlace;
        private Label lblDate;
        private Label lblAttrValue;
        private ComboBox cmbEventType;
        private TextField txtEventPlace;
        private Label lblCause;
        private ComboBox txtEventCause;
        private Label lblOrg;
        private ComboBox txtEventOrg;
        private ComboBox txtAttribute;
        private Button btnPlaceAdd;
        private Button btnPlaceDelete;
        private Button btnAge;
        private GKDateControl dateCtl;
        private Label lblAge;
        private TextField txtAge;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            tabsData = new TabView();
            pageCommon = new TabView.Tab();
            dateCtl = new GKDateControl();
            lblEvent = new Label();
            lblPlace = new Label();
            lblDate = new Label();
            lblCause = new Label();
            lblOrg = new Label();
            lblAttrValue = new Label();
            btnPlaceAdd = new Button();
            btnPlaceDelete = new Button();
            cmbEventType = new ComboBox();
            txtEventPlace = new TextField();
            txtEventCause = new ComboBox();
            txtEventOrg = new ComboBox();
            txtAttribute = new ComboBox();
            pageNotes = new TabView.Tab();
            pageMultimedia = new TabView.Tab();
            pageSources = new TabView.Tab();
            btnAddress = new Button();
            btnAge = new Button();
            lblAge = new Label();
            txtAge = new TextField();

            btnAddress.Width = 16;
            btnAddress.TabIndex = 1;
            btnAddress.Clicked += btnAddress_Click;

            btnAccept.Width = 16;
            btnAccept.TabIndex = 2;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Width = 16;
            btnCancel.TabIndex = 3;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnAddress);
            AddButton(btnAccept);
            AddButton(btnCancel);

            tabsData.AddTab(pageCommon, true);
            tabsData.AddTab(pageNotes, false);
            tabsData.AddTab(pageMultimedia, false);
            tabsData.AddTab(pageSources, false);
            tabsData.X = 0;
            tabsData.Y = 0;
            tabsData.Width = 78;
            tabsData.Height = 32;
            tabsData.TabIndex = 0;

            var pgComView = new View();
            pgComView.Add(lblEvent);
            pgComView.Add(lblPlace);
            pgComView.Add(lblDate);
            pgComView.Add(lblCause);
            pgComView.Add(lblOrg);
            pgComView.Add(lblAttrValue);
            pgComView.Add(btnPlaceAdd);
            pgComView.Add(btnPlaceDelete);
            pgComView.Add(cmbEventType);
            pgComView.Add(txtEventPlace);
            pgComView.Add(txtEventCause);
            pgComView.Add(txtEventOrg);
            pgComView.Add(txtAttribute);
            pgComView.Add(btnAge);
            pgComView.Add(lblAge);
            pgComView.Add(txtAge);
            pgComView.Add(dateCtl);
            pageCommon.View = pgComView;

            lblEvent.X = 1;
            lblEvent.Y = 1;
            lblEvent.TabIndex = 0;

            cmbEventType.X = 1;
            cmbEventType.Y = 2;
            cmbEventType.Width = 32;
            cmbEventType.TabIndex = 1;
            cmbEventType.SelectedItemChanged += EditEventType_SelectedIndexChanged;

            lblAttrValue.X = 1;
            lblAttrValue.Y = 4;
            lblAttrValue.TabIndex = 3;

            txtAttribute.X = 1;
            txtAttribute.Y = 5;
            txtAttribute.Width = 74;
            txtAttribute.TabIndex = 4;

            lblPlace.X = 1;
            lblPlace.Y = 7;
            lblPlace.TabIndex = 5;

            txtEventPlace.X = 1;
            txtEventPlace.Y = 8;
            txtEventPlace.Width = 61;
            txtEventPlace.TabIndex = 6;
            txtEventPlace.KeyDown += EditEventPlace_KeyDown;

            btnPlaceAdd.X = 64;
            btnPlaceAdd.Y = 8;
            btnPlaceAdd.Width = 5;
            btnPlaceAdd.TabIndex = 7;
            btnPlaceAdd.Clicked += btnPlaceAdd_Click;
            btnPlaceAdd.Text = "+";

            btnPlaceDelete.X = 70;
            btnPlaceDelete.Y = 8;
            btnPlaceDelete.Width = 5;
            btnPlaceDelete.TabIndex = 9;
            btnPlaceDelete.Clicked += btnPlaceDelete_Click;
            btnPlaceDelete.Text = "x";

            lblDate.X = 1;
            lblDate.Y = 10;
            lblDate.Width = 6;
            lblDate.TabIndex = 10;

            dateCtl.X = 1;
            dateCtl.Y = 11;
            dateCtl.TabIndex = 10;

            lblAge.X = 1;
            lblAge.Y = 17;
            lblAge.TabIndex = 18;

            txtAge.X = 1;
            txtAge.Y = 18;
            txtAge.Width = 62;
            txtAge.TabIndex = 19;

            btnAge.X = 64;
            btnAge.Y = 18;
            btnAge.Width = 11;
            btnAge.TabIndex = 20;
            btnAge.Clicked += btnAge_Click;

            lblCause.X = 1;
            lblCause.Y = 20;
            lblCause.TabIndex = 18;

            txtEventCause.X = 1;
            txtEventCause.Y = 21;
            txtEventCause.Width = 74;
            txtEventCause.TabIndex = 19;

            lblOrg.X = 1;
            lblOrg.Y = 23;
            lblOrg.TabIndex = 20;

            txtEventOrg.X = 1;
            txtEventOrg.Y = 24;
            txtEventOrg.Width = 74;
            txtEventOrg.TabIndex = 21;

            Width = 80;
            Height = 36;
            Add(tabsData);
        }
    }
}
