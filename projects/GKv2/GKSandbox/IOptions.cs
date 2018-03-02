using System;
using System.Drawing;

namespace GKSandbox
{
    public interface IOptions
    {
        void Accept();
        void Cancel();
        bool HasValidationErrors();
        string DisplayName { get; }
        string TreePosition { get; }
        Image MenuIcon { get; }
    }
}
