using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
{
    partial class OrganizerWin
    {
        private TabControl tabsData;
        private TabPage pageAddresses;
        private TabPage pageTelephones;
        private TabPage pageMails;
        private TabPage pageWebs;

        private void InitializeComponent()
        {
            SuspendLayout();

            pageAddresses = new TabPage();
            pageAddresses.Text = "pageAddresses";

            pageTelephones = new TabPage();
            pageTelephones.Text = "pageTelephones";

            pageMails = new TabPage();
            pageMails.Text = "pageMails";

            pageWebs = new TabPage();
            pageWebs.Text = "pageWebs";

            tabsData = new TabControl();
            tabsData.Pages.Add(pageAddresses);
            tabsData.Pages.Add(pageTelephones);
            tabsData.Pages.Add(pageMails);
            tabsData.Pages.Add(pageWebs);

            Content = tabsData;

            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "OrganizerWin";
            Load += OrganizerWin_Load;
            KeyDown += OrganizerWin_KeyDown;

            UIHelper.SetPredefProperties(this, 780, 540);
            ResumeLayout();
        }
    }
}
