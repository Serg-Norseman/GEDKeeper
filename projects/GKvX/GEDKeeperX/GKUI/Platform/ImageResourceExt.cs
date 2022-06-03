using System;
using GKCore;
using Xamarin.Forms;
using Xamarin.Forms.Xaml;

namespace GKUI.Platform
{
    //[Preserve(AllMembers = true)]
    [ContentProperty("Source")]
    public class ImageResourceExt : IMarkupExtension
    {
        public string Source { get; set; }

        public object ProvideValue(IServiceProvider serviceProvider)
        {
            if (Source == null) {
                return null;
            }

            string resName = Source;//Application.Current.Resources[Source] as string;
            var imageSource = ImageSource.FromResource(resName, typeof(GKUtils).Assembly);
            return imageSource;
        }
    }
}
