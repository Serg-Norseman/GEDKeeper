namespace GKUI
{
    public interface IPlatformSpecifics
    {
        void CloseApplication();
        string GetExternalStorageDirectory();
    }
}
