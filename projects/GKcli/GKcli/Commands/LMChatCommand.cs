/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Threading;
using System.Threading.Tasks;
using GKCore;
using GKCortex.LMChat;
using GKUI.Platform;
using Microsoft.Extensions.Configuration;
using Sharprompt;

namespace GKcli.Commands;

internal class LMChatCommand : BaseCommand
{
    private readonly string _localAPI;
    private readonly string _localModel;
    private readonly bool _stream;

    public LMChatCommand() : base("lm_chat", CLS.LMChat, CommandCategory.Service)
    {
        var config = new ConfigurationBuilder()
            .SetBasePath(GKUtils.GetBinPath())
            .AddJsonFile("appsettings.json")
            .Build();

        _localAPI = config.GetSection("LMSettings:LocalAPI").Value;
        _localModel = config.GetSection("LMSettings:LocalModel").Value;
        _stream = bool.Parse(config.GetSection("LMSettings:Stream").Value);
    }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var client = new LMChatClient(_localAPI, _localModel);
        client.AddHistory("system", "You are a helpful AI assistant in a desktop application.");

        PromptHelper.WriteLine("Chat started. Type 'quit' to exit.");

        // Create a cancellation token source with a 5-minute timeout (300 seconds)
        using var cts = new CancellationTokenSource(TimeSpan.FromMinutes(5));

        RunChatLoop(client, _stream, cts.Token).Wait();
    }

    private async Task RunChatLoop(LMChatClient client, bool stream, CancellationToken cancellationToken)
    {
        while (true) {
            var userInput = Prompt.Input<string>("You");
            if (string.IsNullOrEmpty(userInput) || userInput.ToLower() == "quit") break;

            try {
                client.AddHistory("user", userInput);

                Console.Write("Assistant: ");
                string fullResponse = "";
                if (stream) {
                    // Streaming (real-time output by letters/words)
                    var tokenStream = await client.SendMessageStreamAsync(cancellationToken);

                    await foreach (var token in tokenStream) {
                        if (cancellationToken.IsCancellationRequested)
                            break;
                        Console.Write(token);
                        fullResponse += token; // Collect the full response
                    }
                } else {
                    // Regular request (if streaming is not needed)
                    fullResponse = await client.SendMessageAsync(cancellationToken);
                    Console.Write(fullResponse);
                }
                client.AddHistory("assistant", fullResponse);
                Console.WriteLine();
            } catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested) {
                PromptHelper.WriteLine("Error: Model response took too long (5-minute limit exceeded).");
            } catch (Exception ex) {
                PromptHelper.WriteLine($"An error occurred: {ex.Message}");
            }
        }
    }
}
