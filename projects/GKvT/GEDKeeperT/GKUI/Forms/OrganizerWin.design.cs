#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class OrganizerWin
    {
        private TabView tabsData;
        private TabPage pageAddresses;
        private TabPage pageTelephones;
        private TabPage pageMails;
        private TabPage pageWebs;

        private void InitializeComponent()
        {
            pageAddresses = new TabPage();
            pageTelephones = new TabPage();
            pageMails = new TabPage();
            pageWebs = new TabPage();

            tabsData = new TabView();
            tabsData.AddTab(pageAddresses, true);
            tabsData.AddTab(pageTelephones, false);
            tabsData.AddTab(pageMails, false);
            tabsData.AddTab(pageWebs, false);
            tabsData.Width = Dim.Fill();
            tabsData.Height = Dim.Fill();
            Add(tabsData);

            Width = 78;
            Height = 24;
            Loaded += OrganizerWin_Load;
        }
    }
}
