using System.IO;

namespace GKCommon
{
    public sealed class FileHelper
    {
        public static string GetFileExtension(string fileName)
        {
            string extension = Path.GetExtension(fileName);
            return string.IsNullOrEmpty(extension) ? string.Empty : extension.ToLowerInvariant();
        }
    }
}
