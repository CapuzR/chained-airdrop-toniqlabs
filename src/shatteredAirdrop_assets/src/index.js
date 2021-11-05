import { shatteredAirdrop } from "../../declarations/shatteredAirdrop";

document.getElementById("clickMeBtn").addEventListener("click", async () => {
  const name = document.getElementById("name").value.toString();
  // Interact with shatteredAirdrop actor, calling the greet method
  const greeting = await shatteredAirdrop.greet(name);

  document.getElementById("greeting").innerText = greeting;
});
