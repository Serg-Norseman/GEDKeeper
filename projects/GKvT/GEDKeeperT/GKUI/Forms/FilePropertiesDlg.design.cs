using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class FilePropertiesDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private TabView.Tab pageAuthor;
        private Label lblName;
        private Label lblAddress;
        private Label lblTelephone;
        private TextField txtName;
        private TextField txtTel;
        private TextView txtAddress;
        private TabView.Tab pageOther;
        private TabView PageControl1;
        private GKListView lvRecordStats;
        private ComboBox txtLanguage;
        private Label lblLanguage;

        private void InitializeComponent()
        {
            lblName = new Label() { X = 1, Y = 1, AutoSize = true, TabIndex = 0};
            lblTelephone = new Label() { X = 1, Y = 3, AutoSize = true, TabIndex = 2 };
            lblAddress = new Label() { X = 1, Y = 5, AutoSize = true, TabIndex = 4};
            lblLanguage = new Label() { X = 1, Y = 11, AutoSize = true, TabIndex = 6};

            txtName = new TextField() { X = 1, Y = 2, TabIndex = 1, Width = 54, Height = 1 };
            txtTel = new TextField() { X = 1, Y = 4, TabIndex = 3, Width = 54, Height = 1 };
            txtAddress = new TextView() { X = 1, Y = 6, TabIndex = 5, Width = 54, Height = 5, Multiline = true };
            txtLanguage = new ComboBox() { X = 1, Y = 12, TabIndex = 7, Width = 54, Height = 1 };

            var container = new View();
            container.Border = new Border() { BorderStyle = BorderStyle.None };
            container.Height = Dim.Fill();
            container.Width = Dim.Fill();
            container.Add(lblName);
            container.Add(lblAddress);
            container.Add(lblLanguage);
            container.Add(lblTelephone);
            container.Add(txtName);
            container.Add(txtLanguage);
            container.Add(txtTel);
            container.Add(txtAddress);

            pageAuthor = new TabView.Tab();
            pageAuthor.View = container;

            lvRecordStats = new GKListView();
            lvRecordStats.FullRowSelect = true;
            lvRecordStats.MultiSelect = false;
            lvRecordStats.Width = Dim.Fill();
            lvRecordStats.Height = Dim.Fill();
            lvRecordStats.TabIndex = 1;

            pageOther = new TabView.Tab();
            pageOther.View = lvRecordStats;

            btnAccept = new Button();
            btnAccept.TabIndex = 1;
            btnAccept.MouseClick += AcceptClickHandler;

            btnCancel = new Button();
            btnCancel.TabIndex = 2;
            btnCancel.MouseClick += CancelClickHandler;

            PageControl1 = new TabView();
            PageControl1.AddTab(pageAuthor, true);
            PageControl1.AddTab(pageOther, false);
            PageControl1.Width = 58;
            PageControl1.Height = 18;
            PageControl1.TabIndex = 0;
            Add(PageControl1);

            AddButton(btnAccept);
            AddButton(btnCancel);
            Width = 60;
            Height = 21;
        }
    }
}
