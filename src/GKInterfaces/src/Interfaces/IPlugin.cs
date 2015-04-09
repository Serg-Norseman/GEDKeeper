using System;
using System.ComponentModel;

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

		void OnHostClosing(object sender, CancelEventArgs e);
		void OnHostActivated(object sender, EventArgs e);
		void OnHostDeactivate(object sender, EventArgs e);
        void OnLanguageChange();
    }
}
