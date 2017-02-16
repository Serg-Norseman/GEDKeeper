using System.Security.Principal;

namespace GKCommon
{
    /// <summary>
    /// Description of Utils.
    /// </summary>
    public static class Utils
    {
        /// <summary>
        /// Gets whether the current process (and, by extension, the user) have admin privileges
        /// </summary>
        public static bool IsAdmin()
        {
            using (var identity = WindowsIdentity.GetCurrent())
                return new WindowsPrincipal(identity).IsInRole(WindowsBuiltInRole.Administrator);
        }
    }
}
