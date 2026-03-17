#pragma warning disable IDE1006 // Naming Styles

using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class SourceEditDlg
    {
        private Button btnAccept;
        private Button btnCancel;
        private TabView tabsData;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageRepositories;
        private TabPage pageText;
        private TextView txtText;
        private TabPage pageCommon;
        private Label lblShortTitle;
        private TextField txtShortTitle;
        private Label lblAuthor;
        private TextView txtAuthor;
        private Label lblTitle;
        private TextView txtTitle;
        private Label lblPublication;
        private TextView txtPublication;
        private Label lblDate;
        private GKDateControl dateCtl;
        private TabPage pageUserRefs;

        private void InitializeComponent()
        {
            btnAccept = new Button();
            btnCancel = new Button();
            tabsData = new TabView();
            pageCommon = new TabPage();
            lblShortTitle = new Label();
            lblAuthor = new Label();
            lblTitle = new Label();
            lblPublication = new Label();
            txtShortTitle = new TextField();
            txtAuthor = new TextView();
            txtTitle = new TextView();
            txtPublication = new TextView();
            pageText = new TabPage();
            txtText = new TextView();
            pageRepositories = new TabPage();
            pageNotes = new TabPage();
            pageMultimedia = new TabPage();
            lblDate = new Label();
            dateCtl = new GKDateControl();
            pageUserRefs = new TabPage();

            tabsData.AddTab(pageCommon, true);
            tabsData.AddTab(pageText, false);
            tabsData.AddTab(pageRepositories, false);
            tabsData.AddTab(pageNotes, false);
            tabsData.AddTab(pageMultimedia, false);
            tabsData.AddTab(pageUserRefs, false);
            tabsData.Location = new Point(0, 0);
            tabsData.Size = new Size(62, 29);
            tabsData.TabIndex = 0;

            var pageComCont = new View();
            pageComCont.Add(lblShortTitle);
            pageComCont.Add(lblAuthor);
            pageComCont.Add(lblTitle);
            pageComCont.Add(lblPublication);
            pageComCont.Add(txtShortTitle);
            pageComCont.Add(txtAuthor);
            pageComCont.Add(txtTitle);
            pageComCont.Add(txtPublication);
            pageComCont.Add(lblDate);
            pageComCont.Add(dateCtl);
            pageCommon.View = pageComCont;

            lblShortTitle.Location = new Point(0, 0);
            lblShortTitle.TabIndex = 0;

            lblAuthor.Location = new Point(0, 2);
            lblAuthor.TabIndex = 2;

            lblTitle.Location = new Point(0, 7);
            lblTitle.TabIndex = 4;

            lblPublication.Location = new Point(0, 12);
            lblPublication.TabIndex = 6;

            txtShortTitle.Location = new Point(18, 0);
            txtShortTitle.Size = new Size(40, 1);
            txtShortTitle.TabIndex = 1;
            txtShortTitle.TextChanged += EditShortTitle_TextChanged;

            txtAuthor.Location = new Point(18, 2);
            txtAuthor.Multiline = true;
            txtAuthor.Size = new Size(40, 4);
            txtAuthor.TabIndex = 3;

            txtTitle.Location = new Point(18, 7);
            txtTitle.Multiline = true;
            txtTitle.Size = new Size(40, 4);
            txtTitle.TabIndex = 5;

            txtPublication.Location = new Point(18, 12);
            txtPublication.Multiline = true;
            txtPublication.Size = new Size(40, 4);
            txtPublication.TabIndex = 7;

            lblDate.Location = new Point(0, 17);
            lblDate.TabIndex = 10;

            dateCtl.Location = new Point(18, 17);
            dateCtl.Size = new Size(40, 6);
            dateCtl.TabIndex = 10;

            txtText.Height = Dim.Fill();
            txtText.Width = Dim.Fill();
            txtText.Location = new Point(0, 0);
            txtText.Multiline = true;
            txtText.TabIndex = 0;
            pageText.View = txtText;

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 1;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 2;
            btnCancel.Clicked += CancelClickHandler;

            Size = new Size(64, 33);
            Add(tabsData);
            AddButton(btnAccept);
            AddButton(btnCancel);
        }
    }
}
