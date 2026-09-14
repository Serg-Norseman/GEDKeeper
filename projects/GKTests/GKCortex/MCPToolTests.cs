using GKCore;
using GKCore.Locales;
using GKMCPPlugin;

namespace GKCortex.Tests;

public class MCPToolTests
{
    protected readonly BaseContext fContext;

    static MCPToolTests()
    {
        LangMan.DefInit();
        Plugin.InitFeatures(embedded: false, pureMode: false, tdeMode: true, ragMode: true);
    }

    public MCPToolTests()
    {
        fContext = new BaseContext(null);
    }
}
