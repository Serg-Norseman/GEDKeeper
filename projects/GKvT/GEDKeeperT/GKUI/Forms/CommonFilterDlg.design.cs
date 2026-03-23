#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class CommonFilterDlg
    {
        protected Button btnAccept;
        protected Button btnCancel;
        protected TabView tabsFilters;
        protected TabPage pageFieldsFilter;
        protected Button btnReset;

        private void InitializeComponent()
        {
            btnReset = new Button();
            btnAccept = new Button();
            btnCancel = new Button();
            tabsFilters = new TabView();
            pageFieldsFilter = new TabPage();

            btnReset.Size = new Size(16, 1);
            btnReset.TabIndex = 6;
            btnReset.Clicked += btnReset_Click;

            btnAccept.Size = new Size(16, 1);
            btnAccept.TabIndex = 4;
            btnAccept.Clicked += AcceptClickHandler;

            btnCancel.Size = new Size(16, 1);
            btnCancel.TabIndex = 5;

            tabsFilters.AddTab(pageFieldsFilter);
            tabsFilters.Location = new Point(0, 0);
            tabsFilters.Size = new Size(88, 26);
            tabsFilters.TabIndex = 1;

            Size = new Size(90, 30);
            Add(tabsFilters);
            AddButton(btnReset);
            AddButton(btnAccept);
            AddButton(btnCancel);
        }
    }
}
