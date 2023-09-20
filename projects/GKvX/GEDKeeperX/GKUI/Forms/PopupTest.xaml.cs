using System;
using Xamarin.Forms.Xaml;

namespace GKUI.Forms
{
    [XamlCompilation(XamlCompilationOptions.Compile)]
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
