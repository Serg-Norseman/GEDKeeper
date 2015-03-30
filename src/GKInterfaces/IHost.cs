using System;
using System.Windows.Forms;

namespace GKCore.Interfaces
{
    public interface IHost
    {
		ILangMan CreateLangMan(object sender);
    	IBase GetCurrentFile(bool extMode = false);

    	void BaseChanged(IBase aBase);
    	void NotifyRecord(IBase aBase, object record, RecordAction action);
    	
    	// need BaseClosed! - for TextSearch
    	// need BaseChanged! - for TextSearch
    	
    	string GetAppDataPath();

        bool Register(IPlugin plugin);
        void LogWrite(string msg);

        void WidgetShow(IWidget widget);
        void WidgetClose(IWidget widget);
        
        void ShowMDI(Form form);
    }
}
