using Terminal.Gui;

namespace GKUI.Forms
{
    partial class OrganizerWin
    {
        private TabView tabsData;
        private TabView.Tab pageAddresses;
        private TabView.Tab pageTelephones;
        private TabView.Tab pageMails;
        private TabView.Tab pageWebs;

        private void InitializeComponent()
        {
            pageAddresses = new TabView.Tab();
            pageTelephones = new TabView.Tab();
            pageMails = new TabView.Tab();
            pageWebs = new TabView.Tab();

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
