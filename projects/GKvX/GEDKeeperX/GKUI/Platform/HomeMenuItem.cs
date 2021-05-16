namespace GKUI.Platform
{
    public enum MenuItemType
    {
        Browse,
        About,
        UserRef
    }

    public class HomeMenuItem
    {
        public MenuItemType Id { get; set; }

        public string Title { get; set; }
    }
}
