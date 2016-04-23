defmodule Ollivander.WebEventChannel do
  use Ollivander.Web, :channel

  def join("web_events:" <> subtopic, payload, socket) do
    if authorized?(payload) do
      {:ok, assign(socket, :subtopic, subtopic)}
    else
      {:error, %{reason: "unauthorized"}}
    end
  end
  #def join("web_events:" <> subtopic, payload, socket) do
  #    :timer.send_interval(5_000, :ping)
  #  {:ok, socket}
  #end

  # Channels can be used in a request/response fashion
  # by sending replies to requests from the client
  def handle_in("ping", payload, socket) do
    {:reply, {:ok, payload}, socket}
  end

  def handle_info(:ping, socket) do
    count = socket.assigns[:count] || 1
    push socket, "ping", %{count: count}
    {:noreply, assign(socket, :count, count + 1)}
  end

  # It is also common to receive messages from the client and
  # broadcast to everyone in the current topic (web_events:lobby).
  def handle_in("shout", payload, socket) do
    broadcast socket, "shout", payload
    {:noreply, socket}
  end

  # This is invoked every time a notification is being broadcast
  # to the client. The default implementation is just to push it
  # downstream but one could filter or change the event.
  def handle_out(event, payload, socket) do
    push socket, event, payload
    {:noreply, socket}
  end

  # Add authorization logic here as required.
  defp authorized?(_payload) do
    true
  end
end
