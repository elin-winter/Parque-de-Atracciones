%%%%%%%%%%%%%%%%%%% Parque de Atracciones %%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%% Personas, parques y atracciones

persona(nina, joven, 22, 1.60).
persona(marcos, ninio, 8, 1.32).
persona(osvaldo, adolescente, 13, 1.29).

% atraccion(parque, nombre, edadMinima, alturaMinima)
atraccion(parqueDeLaCosta,trenFantasma).
atraccion(parqueDeLaCosta,montaniaRusa).
atraccion(parqueDeLaCosta,maquinaTiquetera).
atraccion(parqueAcuatico,toboganGigante).
atraccion(parqueAcuatico,rioLento).
atraccion(parqueAcuatico,piscinaDeOlas).

requisito(trenFantasma,edad(12)).
requisito(montaniaRusa,altura(1.3)).
requisito(toboganGigante,altura(1.5)).
requisito(piscinaDeOlas,edad(5)).

% Punto 1

puedeSubir(Persona, Atraccion):-
    persona(Persona,_,_,_),
    atraccion(_,Atraccion),
    forall(requisito(Atraccion,Requisito),cumpleRequisito(Persona,Requisito)).
    
cumpleRequisito(Persona,edad(Minima)):- 
    persona(Persona, _, E,_), E >= Minima.

cumpleRequisito(Persona,altura(Minima)):- 
    persona(Persona,_, _,A), A >= Minima.

% Punto 2

esParaElle(Parque, Persona):-
    % persona(Persona,_,_,_,_),   % es necesario ?
    atraccion(Parque, _), 
    forall(atraccion(Parque, Atraccion), puedeSubir(Persona, Atraccion)).

% Punto 3

malaIdea(GrupoEtario, Parque):-
    atraccion(Parque, Atraccion),
    not((persona(Persona, GrupoEtario, _, _), puedeSubir(Persona, Atraccion))).

%%%%%%%%%%%%%%% Programas

% Punto 1

programaLogico(Programa) :-
    atraccion(Parque, _),
    forall(member(Atraccion, Programa), atraccion(Parque, Atraccion)),
    noHayRepetidos(Programa).

noHayRepetidos([]).
noHayRepetidos([Atraccion | Atracciones]) :-
    noHayRepetidos(Atracciones),
    not(member(Atraccion, Atracciones)).

% Punto 2

hastaAca(_, [], []).

hastaAca(Persona, [Atraccion | Atracciones], [Atraccion | Subprograma]):-
    puedeSubir(Persona, Atraccion),
    hastaAca(Persona, Atracciones, Subprograma).

hastaAca(Persona, [Atraccion | _], []):-
    not(puedeSubir(Persona, Atraccion)).

%%%%%%%%%%%%%%% Pasaportes

juegoComun(trenFantasma, 1000).
juegoComun(maquinaTiquerera, 50).
juegoPremium(montaniaRusa).
juegoPremium(rioLento).

pasaporte(basico(50000), nina).
pasaporte(basico(20), marcos).
pasaporte(flex(4000, montaniaRusa), osvaldo).
pasaporte(flex(30, rioLento), juan).
pasaporte(premium, miriam).
pasaporte(premium, alejandra).

puedeSubir2(Persona, Atraccion) :-
    puedeSubir(Persona, Atraccion),
    pasaporte(Pasaporte, Persona), 
    cumpleRequisitosPasaporte(Pasaporte, Atraccion).

cumpleRequisitosPasaporte(basico(Creditos), Atraccion) :-
    juegoComun(Atraccion, Valor),
    Valor < Creditos.

cumpleRequisitosPasaporte(flex(Creditos, _), Atraccion) :-
    juegoComun(Atraccion, Valor),
    Valor < Creditos.

cumpleRequisitosPasaporte(premium, Atraccion) :-
    juegoComun(Atraccion, _).

cumpleRequisitosPasaporte(premium, Atraccion) :-
    juegoPremium(Atraccion).

cumpleRequisitosPasaporte(flex(_, Atraccion), Atraccion) :-
    juegoPremium(Atraccion).
