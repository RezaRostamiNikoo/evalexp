document.addEventListener("DOMContentLoaded", (event: Event) => {
    (document.forms as any)["expression-form"].addEventListener("submit", function (event: any) {
        event.preventDefault()
    });
    document.getElementById("btn")?.addEventListener("click", (event: Event) => {
        doit();
    })
});

function doit() {
    const expression = document.getElementById("expression_field");

    alert("asdasdasd");
}

