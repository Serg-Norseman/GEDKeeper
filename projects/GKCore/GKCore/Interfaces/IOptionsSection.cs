using System;

namespace GKCore.Interfaces
{
    public interface IOptionsSection
    {
        void Accept();
        void Cancel();
        bool HasValidationErrors();
        string DisplayName { get; }
        string TreePosition { get; }
        IImage MenuIcon { get; }
    }
}
