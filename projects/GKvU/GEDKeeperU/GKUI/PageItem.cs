using System;

namespace GEDKeeperU
{
    public class PageItem
    {
        public string Tag;
        public Type Page;

        public PageItem(string tag, Type page)
        {
            Tag = tag;
            Page = page;
        }
    }
}
