using Terminal.Gui.ViewBase;
using Terminal.Gui.Views;

namespace GKUI.Forms
{
    partial class OrganizerWin
    {
        private TabView tabsData;
        private Tab pageAddresses;
        private Tab pageTelephones;
        private Tab pageMails;
        private Tab pageWebs;

        private void InitializeComponent()
        {
            pageAddresses = new Tab();
            pageTelephones = new Tab();
            pageMails = new Tab();
            pageWebs = new Tab();

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
        }
    }
}
