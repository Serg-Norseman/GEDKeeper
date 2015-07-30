using System;

namespace GKCore.Interfaces
{
    public interface IPlugin
    {
        string DisplayName { get; } // отображаемое имя плагина
        IHost Host { get; } // ссылка на главную форму
        ILangMan LangMan { get; }

        void Execute();
        bool Startup(IHost host);
        bool Shutdown();

		void OnHostClosing(ref bool cancelClosing);
		void OnHostActivate();
		void OnHostDeactivate();
        void OnLanguageChange();
    }
}
