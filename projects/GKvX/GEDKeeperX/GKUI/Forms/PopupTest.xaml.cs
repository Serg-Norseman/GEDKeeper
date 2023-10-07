using System;

namespace GKUI.Forms
{
    public partial class PopupTest : Xamarin.CommunityToolkit.UI.Views.Popup
    {
        public PopupTest()
        {
            InitializeComponent();

            IsLightDismissEnabled = false;
        }

        public void CancelAttendanceClicked(object sender, EventArgs e)
        {
            Dismiss(null);
        }

        public void ConfirmAttendanceClicked(object sender, EventArgs e)
        {
        }
    }
}
