using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class ChronicleWin
    {
        private GKListView lvEvents;

        private void InitializeComponent()
        {
            lvEvents = new GKListView();
            lvEvents.Width = Dim.Fill();
            lvEvents.Height = Dim.Fill();
            Add(lvEvents);

            Width = 80;
            Height = 24;
        }
    }
}
