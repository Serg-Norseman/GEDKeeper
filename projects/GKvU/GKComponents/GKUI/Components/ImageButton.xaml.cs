using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Media;

namespace GKUI.Components
{
    public sealed partial class ImageButton : Button
    {
        public ImageSource Image
        {
            get { return btnImage.Source; }
            set { btnImage.Source = value; }
        }

        public string Text
        {
            get { return btnText.Text; }
            set { btnText.Text = value; }
        }

        public ImageButton()
        {
            InitializeComponent();
        }
    }
}
