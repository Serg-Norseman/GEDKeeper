using GKCore;
using System;
using System.IO;
using System.Reflection;
using Windows.Storage.Streams;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Media;
using Windows.UI.Xaml.Media.Imaging;

namespace GKUI.Components
{
    public static class UIHelper
    {
        public static StackPanel CreateHStackLayout(params UIElement[] items)
        {
            return CreateStackLayout(Orientation.Horizontal, 0, 10, items);
        }

        public static StackPanel CreateVStackLayout(params UIElement[] items)
        {
            return CreateStackLayout(Orientation.Vertical, 0, 10, items);
        }

        public static StackPanel CreateStackLayout(Orientation orientation,
                                                    int padding, int spacing,
                                                    params UIElement[] items)
        {
            var res = new StackPanel();
            res.Orientation = orientation;
            res.Padding = new Thickness(padding);
            res.Spacing = spacing;
            res.HorizontalAlignment = HorizontalAlignment.Center;
            foreach (var itm in items)
            {
                res.Children.Add(itm);
            }
            return res;
        }

        public static void SetGridElement(this Grid grid, FrameworkElement element, int col, int row)
        {
            Grid.SetColumn(element, col);
            Grid.SetRow(element, row);
            grid.Children.Add(element);
        }

        public static ImageButton CreateDialogButton(string title, RoutedEventHandler clickHandler)
        {
            var btn = new ImageButton();
            //btn.ContentLayout = new Button.ButtonContentLayout(Button.ButtonContentLayout.ImagePosition.Left, 10);
            btn.VerticalAlignment = VerticalAlignment.Center;
            btn.HorizontalAlignment = HorizontalAlignment.Center;
            btn.Content = title;
            btn.Click += clickHandler;
            return btn;
        }

        public static ImageSource LoadResourceImage(string resName)
        {
            Stream embeddedResourceStream = GKUtils.LoadResourceStream(resName);
            byte[] bytes = new byte[embeddedResourceStream.Length];
            embeddedResourceStream.Read(bytes, 0, (int)embeddedResourceStream.Length);

            InMemoryRandomAccessStream randStream = new InMemoryRandomAccessStream();
            Stream stream = randStream.AsStreamForWrite();
            stream.Write(bytes, 0, bytes.Length);
            stream.Flush();
            stream.Seek(0, SeekOrigin.Begin);

            BitmapImage bitmapImage = new BitmapImage();
            bitmapImage.SetSource(randStream);
            return bitmapImage;
        }

        public static ImageSource LoadResourceImage(Type baseType, string resName)
        {
            Stream embeddedResourceStream = GKUtils.LoadResourceStream(baseType, resName);
            byte[] bytes = new byte[embeddedResourceStream.Length];
            embeddedResourceStream.Read(bytes, 0, (int)embeddedResourceStream.Length);

            InMemoryRandomAccessStream randStream = new InMemoryRandomAccessStream();
            Stream stream = randStream.AsStreamForWrite();
            stream.Write(bytes, 0, bytes.Length);
            stream.Flush();
            stream.Seek(0, SeekOrigin.Begin);

            BitmapImage bitmapImage = new BitmapImage();
            bitmapImage.SetSource(randStream);
            return bitmapImage;
        }
    }
}
