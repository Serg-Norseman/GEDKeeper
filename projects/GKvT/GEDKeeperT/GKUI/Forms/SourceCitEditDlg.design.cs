#pragma warning disable IDE1006 // Naming Styles

using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class SourceCitEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private Label lblPage;
        private TextField txtPage;
        private Label lblSource;
        private Button btnSourceAdd;
        private Label lblCertainty;
        private ComboBox txtCertainty;
        private ComboBox cmbSource;
        private TabView tabsData;
        private TabView.Tab pageCommon;
        private TabView.Tab pageOther;
        private TextView txtText;
        private GKDateControl dateCtl;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            tabsData = new TabView();
            pageCommon = new TabView.Tab();
            lblPage = new Label();
            txtPage = new TextField();
            lblSource = new Label();
            btnSourceAdd = new Button();
            lblCertainty = new Label();
            txtCertainty = new ComboBox();
            cmbSource = new ComboBox();
            pageOther = new TabView.Tab();
            txtText = new TextView();
            dateCtl = new GKDateControl();

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 1;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 2;
            btnCancel.Clicked += CancelClickHandler;

            AddButton(btnAccept);
            AddButton(btnCancel);

            lblSource.Location = new Point(1, 1);
            lblSource.TabIndex = 0;

            cmbSource.Location = new Point(16, 1);
            cmbSource.Size = new Size(35, 2);
            cmbSource.TabIndex = 1;
            cmbSource.KeyUp += cbSource_KeyUp;

            btnSourceAdd.Text = "+";
            btnSourceAdd.Location = new Point(52, 1);
            btnSourceAdd.Size = new Size(5, 1);
            btnSourceAdd.TabIndex = 2;
            btnSourceAdd.TabStop = false;
            btnSourceAdd.Clicked += btnSourceAdd_Click;

            lblPage.Location = new Point(1, 3);
            lblPage.TabIndex = 3;

            txtPage.Location = new Point(16, 3);
            txtPage.Size = new Size(20, 1);
            txtPage.TabIndex = 4;

            lblCertainty.Location = new Point(1, 5);
            lblCertainty.TabIndex = 5;

            txtCertainty.Location = new Point(16, 5);
            txtCertainty.Size = new Size(40, 2);
            txtCertainty.TabIndex = 6;

            var panCommon = new View();
            panCommon.Add(lblPage);
            panCommon.Add(txtPage);
            panCommon.Add(lblSource);
            panCommon.Add(btnSourceAdd);
            panCommon.Add(lblCertainty);
            panCommon.Add(txtCertainty);
            panCommon.Add(cmbSource);
            pageCommon.View = panCommon;

            dateCtl.AutoSize = true;
            dateCtl.Date = null;
            dateCtl.Location = new Point(0, 0);
            dateCtl.Size = new Size(68, 6);
            dateCtl.TabIndex = 2;

            txtText.Location = new Point(0, 6);
            txtText.Multiline = true;
            txtText.Size = new Size(68, 16);
            txtText.TabIndex = 3;

            var panOther = new View();
            panOther.Add(txtText);
            panOther.Add(dateCtl);
            pageOther.View = panOther;

            tabsData.AddTab(pageCommon);
            tabsData.AddTab(pageOther);
            tabsData.Location = new Point(0, 0);
            tabsData.Size = new Size(70, 26);
            tabsData.TabIndex = 0;

            Size = new Size(72, 30);
            Add(tabsData);
        }
    }
}
