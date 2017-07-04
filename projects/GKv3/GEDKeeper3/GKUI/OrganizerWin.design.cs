using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI
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

            ClientSize = new Size(780, 540);
            Maximizable = false;
            Minimizable = false;
            ShowInTaskbar = false;
            Title = "OrganizerWin";
            Load += OrganizerWin_Load;
            KeyDown += OrganizerWin_KeyDown;

            UIHelper.SetControlFont(this, "Tahoma", 8.25f);
            ResumeLayout();
        }
    }
}
