using System;
using Windows.UI.Xaml.Controls;

namespace GKUI
{
    public class PageItem
    {
        public string Tag;
        public Type Page;
        public SymbolIcon Symbol;

        public PageItem(string tag, Type page, SymbolIcon symbol = null)
        {
            Tag = tag;
            Page = page;
            Symbol = symbol;
        }
    }
}
