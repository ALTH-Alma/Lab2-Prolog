
%FPredicado extra extra:
 esListaString([]).
 esListaString([H|T]):- string(H), esListaString(T).

%_________________________________________

%TDA fecha: se usa para tener un modelo estandar para todas fechas que se usaran en los demas TDAs.
%Representación: fecha[entero dia, entero mes,entero año]

%Capa constructor:
fecha(D,M,A,F):- 
integer(D), integer(M), integer(A),
D > 0, D < 32, M > 0, M < 13,
F=[D,M,A].

%Capa pertenencia:
esFecha([D,M,A]):- 
integer(D), integer(M), integer(A),
D > 0, D < 32, M > 0, M < 13.

%_________________________________________

%TDA recompensa: se usa para representar una recompensa en una pregunta.
%representación: recompensa[string nombre oferto, entero monto].

%Capa constructor:
%recompensaVacia= ["",0];

recompensa(O,MR,R):- 
string(O), integer(MR), 
R= [O,MR].

%Capa pertenencia:
esRecompensa([O,MT]):-
string(O), integer(MT), 
MT>0; ([O,MT]==["",0]).

%_________________________________________

%TDA usuario: representa un usuario y sus elementos obligatorios.
%Representación: usuario[string nombre, string contraseña, entero reputación, lista de string referencias].

%Capa constructor: 
%Entrada: un string NU (nombre del usuario), un string CU (contraseña del usuario), un entero mayor que 0 (reputación del usuario),
%una lista de strings LR (lista de referencias del usuario) y una variable para asignar al usuario.
%Salida: un Usuario (lista de elementos) si las entradas son validas y un false sino.

usuario([]).
crearUsuario(NU,CU,RU,LR, USUARIO):-
string(NU), string(CU), integer(RU), esListaString(LR),
RU > -1,
USUARIO= [NU,CU,RU,LR].

%Capa pertenencia:
%Entrada: una lista de elementos que sirve como postulante a un usuario.
%Salida: un booleano, un trus si la lista es un usuario y un false sino.

esUsuario([]).
esUsuario([NU,CU,RU,LR]):-
string(NU), string(CU), integer(RU), esListaString(LR),
RU > -1 .

%_________________________________________

%TDA lista de usuarios: representa una lista de usuarios en el stack.
%Representación: una lista de usuarios [usuario1, usuario2, ......., usuarioN].

%Capa pertenencia:
%Entrada: una lista con elementos, sirve como postulante para una lista de usuarios.
%Salida: un booleano, un true si la lista corresponde a una lista de usuarios.

esListaUserVacia([]).
esListaUsuarios([]).
esListaUsuarios([H|T]):- esUsuario(H),esListaUsuarios(T).


%Función extra: verifica si existe un usuario en la lista de usuarios.
existeUsuario([[NOMBRE,_,_,_]|_], NOMBRE):- !.
existeUsuario([_|Usuarios], NOMBRE):-
existeUsuario(Usuarios, NOMBRE).


%_________________________________________

%TDA respuesta: representa una respuesta y sus elementos obligatorios.
%Representación: respuesta[entero id respuesta, string autor, fecha fecha de publicación, string contenido respuesta, lista etiquetas,
% string estado [Aceptada/Rechazada], entero votos a favor, entero votos en contra, entero reportes].

%Capa constructor:
%Entrada: entero IDR (identificador respuesta), string AR (autor de la respuesta), fecha FP (fecha de publicación), string C (contenido), 
%lista de strings LE (lista de etiquetas), string EA (estado de aceptacion), entero VF (votos a favor), entero VC (votos en contra), 
%entero NR (entero de reportes) y una variable RESPUESTA para asignar la respuesta contruida.
%Salida: un Respuesta (lista de elementos) si las entradas son correctas y un false sino.

respuesta([]).
crearRespuesta(IDR, AR, FP, C, LE, EA, VF, VC, NR, RESPUESTA):-
integer(IDR), string(AR), esFecha(FP), string(C), esListaString(LE), string(EA), integer(VF), integer(VC), integer(NR), 
IDR > -1, VF > -1, VC > -1, NR > -1,
RESPUESTA= [IDR, AR, FP, C, LE, EA, VF, VC, NR].

%Capa pertenencia:
%Entrada: una lista de elementos que sirve como postulante a ser una respuesta.
%Salida: un booleano, un true si la lista es una respuesta y un false sino.

esRespuesta([]).
esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]):-
integer(IDR), string(AR), esFecha(FP), string(C), esListaString(LE), string(EA), integer(VF), integer(VC), integer(NR),
IDR > -1, VF > -1, VC > -1, NR > -1 .


%_________________________________________

%TDA lista de respuestas: representa una lista de respuestas en una pregunta.
%Representación: una lista de respuestas [respuesta1, respuesta2, ......., respuestaN].

%Capa pertenencia:
%Entrada: una lista con elementos, sirve como postulante para una lista de respuestas.
%Salida: un booleano, un true si la lista corresponde a una lista de respuestas y un false sino.
esListaRespuestas([]).
esListaRespuestas([H|T]):- esRespuesta(H),esListaRespuestas(T).

%Capa constructor:
%Entrada: una lista de respuestas o una nueva lista vacia, la nueva respuesta y una variable para asignar la nueva lista de respuestas con la respuesta incluida.
%Salida: una nueva lista de respuestas.

agregarRespuesta([], [IDR, AR, FP, C, LE, EA, VF, VC, NR], [[IDR, AR, FP, C, LE, EA, VF, VC, NR]]):- 
esRespuesta([IDR, AR, FP, C, LE, EA, VF, VC, NR]). %caso base

agregarRespuesta([H|T], [IDR, AR, FP, C, LE, EA, VF, VC, NR], [H|NEWT]):- 
esListaRespuestas([H|T]),
agregarRespuesta(T, [IDR, AR, FP, C, LE, EA, VF, VC, NR], NEWT). 

%_________________________________________

%TDA pregunta: representa una preguntas y sus lementos obligatorios.
%Representación: pregunta[entero id pregunta, string nombre del autor, fecha fecha de publicación, string contenido, lista de strings etiquetas,
%string estado pregunta [Abierta/Cerrada], entero número de visualizaciones, entero votos a favor, entero votos en contra, recompensa Recompensa,
% entero reportes, lista de respuestas respuestas].

%Capa constructor:
%Entrada: un entero ID (identificador de pregunta), un string AP (nombre del autor de la pregunta), una fecha FP (fecha de publicación de la pregunta), 
%un string C (contenido pregunta), una lista de strins LE (listas de etiquetas), un string EP (estado de la pregunta), un entero NV (número de visualizaciones),
%un entero VF (votos a favor), un entero VC (votos en contra), una recompensa REC (recompensa con ofertor y monto), una lista de respuestas RESPUESTAS 
%(repuestas de la pregunta), una variable PREGUNTA para asignar la pregunta construida. 
%Salida: una Pregunta (lista de elementos) si las entradas son correctas y un false sino. 

pregunta([]).
crearPregunta(IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS,PREGUNTA):-
integer(IDP),string(AP),esFecha(FP),string(C),esListaString(LE),string(EP),integer(NV),integer(VF),integer(VC),esRecompensa(REC),integer(NR),esListaRespuestas(RESPUESTAS),
IDP > -1, NV > -1, VF > -1, VC > -1, NR > -1,
PREGUNTA= [IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS].

%Capa pertenencia:
%Entrada: una lista de elementos que sirve como postulante a ser una pregunta.
%Salida: un booleano, un true si la lista es una pregunta y un false sino.

esPregunta([]).
esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]):-
integer(IDP),string(AP),esFecha(FP),string(C),esListaString(LE),string(EP),integer(NV),integer(VF),integer(VC),esRecompensa(REC),integer(NR),esListaRespuestas(RESPUESTAS),
IDP > -1, NV > -1, VF > -1, VC > -1, NR > -1 .

%Capa selector:
%Entrada: todos los predicados de la capa selector reciben como entrada una pregunta.

%Salida: un entero, ID de la pregunta.
getIdPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],IDP):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%Salida: una lista de respuestas, respuestas de la pregunta.
getRespuestasPreg([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],RESPUESTAS):- esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]).

%_________________________________________

%TDA lista de preguntas: representa una lista de preguntas en el stack.
%Representación: una lista de preguntas [pregunta1, pregunta2, ......., preguntaN].

%Capa pertenencia:
%Entrada: una lista con elementos, sirve como postulante para una lista de preguntas.
%Salida: un booleano, un true si la lista corresponde a una lista de preguntas y un false sino.
esListaPreguntasVacia([]).
esListaPreguntas([]).
esListaPreguntas([H|T]):- esPregunta(H),esListaPreguntas(T).

%Capa constructor:
%Entrada: una lista de respuestas o una nueva lista vacia, la nueva respuesta y una variable para asignar la nueva lista de respuestas con la respuesta incluida.
%Salida: una nueva lista de respuestas.

agregarPregunta([], [IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS], [[IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]]):-
esPregunta([IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]). 

agregarPregunta([H|T], [IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS],  [H|NEWT]):- 
esListaPreguntas([H|T]),
agregarPregunta(T, [IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS], NEWT). 

%_________________________________________

%TDA stack: representa a Stack Overflow.
%Representación: stack [usuario Sesión activa, lista de usuarios Registrados, lista de preguntas Preguntas stack, entero Correlativo pregunta, entero Correlativo respuestas].

%Capa constructor:
%Entrada: un usuario UA (usuario activo en el stack), una lista de usuarios LU (usuarios registrados en el stack), una lista de preguntas LP (preguntas del stack),
% un entero CP (correlativo para llevar la cuenta de la cantidad de preguntas en el stack), un entero CR (correlativo para llevar la cuenta de la cantidad de respuestas en el stack).
%Salida: un stack.

%stack([[],[],[],0,0]).
crearStack(UA, LU, LP, CP, CR, STACK):-
esUsuario(UA),esListaUsuarios(LU),esListaPreguntas(LP),integer(CP),integer(CR),
CP > -1 , CR > -1,
STACK= [UA, LU, LP, CP, CR].

%Capa pertenencia:
%Entrada: un lista de elementos que sirven como postulante a ser un stack.
%salida: un boolenao, un true si la lista es un stack y un false sino.

esStack([[],[],[],0,0]).
esStack([UA, LU, LP, CP, CR]):-
esUsuario(UA),esListaUsuarios(LU),esListaPreguntas(LP),integer(CP),integer(CR),
CP > -1 , CR > -1.

%Capa selector:
%Entrada: todos los predicados de la capa selector reciben un stack.

%Salida: un usuario, usuario activo en el stack.
getActivo([UA, LU, LP, CP, CR],UA):- esStack([UA, LU, LP, CP, CR]).

%Salida: una lista de usuarios, registrados en el stack.
getListaUsuarios([UA, LU, LP, CP, CR],LU):- esStack([UA, LU, LP, CP, CR]).

%Salida: una lista de preguntas, preguntas del stack.
getListaPreguntas([UA, LU, LP, CP, CR],LP):- esStack([UA, LU, LP, CP, CR]).

%Salida: un entero, correlativo de preguntas en el stack.
getCorrelativoPreg([UA, LU, LP, CP, CR],CP):- esStack([UA, LU, LP, CP, CR]).

%Salida: un entero, correlativo de respuestas en el stack.
getCorrelativoRes([UA, LU, LP, CP, CR],CR):- esStack([UA, LU, LP, CP, CR]).


%predicados extras:

agregarUserStack([UA, [[NewNameUser,Pass,Reput,Ref]|Usuarios], LP, CP, CR], NewNameUser, NewPass, [UA, [[NewNameUser,Pass,Reput,Ref]|Usuarios], LP, CP, CR]):- !. %Si existe usuario.
agregarUserStack([UA, [], LP, CP, CR], NameUser, Pass, [UA, [[NameUser, Pass, 0, []]], LP, CP, CR]):- string(NameUser),string(Pass).
agregarUserStack([UA, [Usuario|Usuarios], LP, CP, CR], NewNameUser, NewPass, [UA, [Usuario|NewUsuarios], LP, CP, CR]):-
agregarUserStack([UA, Usuarios, LP, CP, CR], NewNameUser, NewPass, [UA, NewUsuarios, LP, CP, CR]).

autentificarUserEnStack([UA, [[NameUser,Pass,Reput,Ref]|Usuarios], LP, CP, CR], NameUser, Pass, [[NameUser,Pass,Reput,Ref], [[NameUser,Pass,Reput,Ref]|Usuarios], LP, CP, CR]):- !. 
autentificarUserEnStack([UA, [Usuario|Usuarios], LP, CP, CR], NameUser, Pass, [NewUA, [Usuario|NewUsuarios], LP, CP, CR]):-
autentificarUserEnStack([UA, Usuarios, LP, CP, CR], NameUser, Pass, [NewUA, NewUsuarios, LP, CP, CR]).

agregarAskStack([[NombreUA,_,_,_], LU, [], CP, CR], FechaP, ContP, Etiq, NewCorrPreg, [[], LU, [[CP, NombreUA, FechaP, ContP, Etiq,"Abierta",0,0,0,["",0],0,[]]], NewCorrPreg, CR]).
agregarAskStack([[NombreUA,_,_,_], LU, [Pregunta|Preguntas], CP, CR], FechaP, ContP, Etiq, NewCorrPreg, [[], LU, [[CP, NombreUA, FechaP, ContP, Etiq,"Abierta",0,0,0,["",0],0,[]], Pregunta|Preguntas], NewCorrPreg, CR]).

%_________________________________________
%Desarrollo requerimiento 2: Hechos.

usuario(["Maria", "Maria1999", 50, ["Racket","c#"]]).
usuario(["Ana","A1234", 70, ["java","python"]]).
usuario(["Juan","juan2000", 20, ["python","c++"]]).
usuario(["Pedro", "P340", 90,  ["java","c#"]]).


pregunta([0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]]).
pregunta([1, "Ana", [29, 10, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Maria", [20, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 15, 2, 0],
			[4, "Juan", [13, 11, 2020], "No se puede hacer.", ["errores"], "Rechazada", 6, 11, 2],
 			[5, "Pedro", [10, 11, 2020], "Usando Qt Style Sheet.", ["imagen"], "Aceptada", 36, 3, 0],
 			[6, "Ana", [3, 11, 2020], "No lo se.", ["imagen"], "Pendiente", 2, 13, 3]
 			]]).
pregunta([2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]]).
pregunta([3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]]).
pregunta([4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]).

respuesta([2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0]).
respuesta([1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1]).
respuesta([0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]).
respuesta([3,"Maria", [20, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 15, 2, 0]).
respuesta([4, "Juan", [13, 11, 2020], "No se puede hacer.", ["errores"], "Rechazada", 6, 11, 2]).
respuesta([5, "Pedro", [10, 11, 2020], "Usando Qt Style Sheet.", ["imagen"], "Aceptada", 36, 3, 0]).
respuesta([6, "Ana", [3, 11, 2020], "No lo se.", ["imagen"], "Pendiente", 2, 13, 3]).
respuesta([7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0]).
respuesta([8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]).
respuesta([9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]).


stack([[],[["Maria", "Maria1999", 50, ["Racket","c#"]],["Ana","A1234", 70, ["java","python"]],["Juan","juan2000", 20, ["python","c++"]],["Pedro", "P340", 90, ["java","c#"]]], 
			[
			[0, "Maria", [29, 2, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 30, 10, 5, ["Maria", 10], 1, 
 			[
 			[2,"Ana", [12, 5, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 15, 2, 0],
 			[1,"Pedro", [22, 3, 2020], "No es una mala practica.", ["Variables globales"], "Rechazada", 5, 6, 1],
 			[0,"Juan", [2, 3, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Pendiente", 20, 3, 0]
 			]],
			[1, "Ana", [29, 10, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Maria", [20, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 15, 2, 0],
			[4, "Juan", [13, 11, 2020], "No se puede hacer.", ["errores"], "Rechazada", 6, 11, 2],
 			[5, "Pedro", [10, 11, 2020], "Usando Qt Style Sheet.", ["imagen"], "Aceptada", 36, 3, 0],
 			[6, "Ana", [3, 11, 2020], "No lo se.", ["imagen"], "Pendiente", 2, 13, 3]
 			]],
 			[2, "Pedro", [2, 12, 2020], "¿Como puedo hacer una lista?", ["listas"], "Abierta", 25, 5, 20, ["Pedro",5], 0,
			[
			[7,"Juan", [4, 12, 2020], "Con recursión.", ["construccion"], "Pendiente", 2, 3, 0],
			[8, "Ana", [3, 12, 2020], "Utilizando ciclos.", ["listas", "ciclos"], "Pendiente", 3, 2, 0]
 			]],
 			[3, "Juan", [3, 12, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 10, 5, 2, ["",0], 0, 
 			[]],
 			[4, "Maria", [4, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 30, 12, 2, ["",0], 0,
			[
			[9,"Ana", [5, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["texto prolog"], "Aceptada", 20, 0, 0]
 			]]], 
 			5, 10]).


stack([[],[["Paola", "P1998", 53, ["Racket","c++"]],["Sam", "S123", 65, ["C#","python"]],["Pablo", "PO2001", 21, ["python","c"]],["Teo", "T342", 90, ["python","java"]]], 
			[
			[0, "Pablo", [28, 3, 2020], "¿Por que es una mala practica usar variables globales?", ["Malas practicas","variables"], "Abierta", 31, 12, 6, ["",0], 1, 
 			[
 			[2,"Sam", [12, 6, 2020], "Existen varias razones.", ["Problemas", "variables"], "Pendiente", 7, 3, 1],
 			[1,"Paola", [13, 4, 2020], "No es del todo una mala practica.", ["Variables globales"], "Rechazada", 6, 3, 0],
 			[0,"Teo", [4, 4, 2020], "Aumenta la complejidad.", ["Variables", "Problemas"], "Aceptada", 21, 2, 0]
 			]],
			[1, "Paola", [9, 9, 2020], "¿Como pongo una imagen de fondo a la ventana creada con PyQT5?", ["python","interfaz-gráfica","imagen"], "Abierta", 50, 5, 2, ["",0], 0,
			[
			[3,"Sam", [3, 11, 2020], "Usando Designer.", ["imagen"], "Aceptada", 25, 3, 0],
			[4, "Pablo", [20, 10, 2020], "No se puede hacer.", ["errores"], "Rechazada", 0, 15, 2],
 			[5, "Teo", [10, 10, 2020], "Usando Qt Style Sheet.", ["imagen"], "Aceptada", 36, 2, 0]
 			]],
 			[2, "Pablo", [5, 11, 2020], "¿Como puedo hacer una lista de diferentes tipos de datos?", ["listas"], "Abierta", 20, 7, 13, ["Pedro",5], 0,
			[
			[6,"Teo", [7, 11, 2020], "Dependera del lenguaje.", ["construccion"], "Pendiente", 10, 2, 0],
			[7, "Paola", [6, 11, 2020], "No siempre se puede.", ["listas", "lenguaje"], "Pendiente", 9, 3, 0]
 			]],
 			[3, "Sam", [30, 11, 2020], "¿Como puedo encontrar permutaciones en C?", ["Permutaciones","C"], "Abierta", 28, 17, 1, ["Sam",20], 0, 
 			[]],
 			[4, "Teo", [2, 12, 2020], "¿Como puedo hacer que en prolog se vea el texto completo?", ["Prolog","texto"], "Abierta", 36, 19, 1, ["",0], 0,
			[
			[8,"Paola", [3, 12, 2020], "Con el comando set_prolog_flag(answer_write_options,[max_depth(0)]).", ["Visualizacion prolog"], "Aceptada", 24, 0, 0],
 			[9, "Pablo", [2, 12, 2020], "Parece que no se puede.", ["Texto prolog"], "Rechazada", 0, 20, 3]
 			]]], 
 			5, 10]).


%_________________________________________

%3Desarrollo predicado register:

register(Stack, NewUserName, PassUser, Stack2):-
esStack(Stack), agregarUserStack(Stack, NewUserName, PassUser, Stack2).


%4Desarroloo predicado login:

login(Stack, UserName, Pass, Stack2):-
esStack(Stack), string(UserName), string(Pass),
autentificarUserEnStack(Stack, UserName, Pass, Stack2).


%5Desarrollo predicado ask:

ask([UsuarioActivo, ListUser, ListPreg, CorrPreg, CorrRes], FechaP, ContP, Etiq, Stack2):-
esStack([UsuarioActivo, ListUser, ListPreg, CorrPreg, CorrRes]), esFecha(FechaP), string(ContP), esListaString(Etiq), NewCorrPreg is CorrPreg + 1,
agregarAskStack([UsuarioActivo, ListUser, ListPreg, CorrPreg, CorrRes], FechaP, ContP, Etiq, NewCorrPreg, Stack2).


%7Desarrollo predicado answer.

	
%Recibe una lista de preguntas.

agregarResPreg([[NUA,CU,RU,REP],LU,[[IDP,A,F,C,LE,E,NV,VF,VC,REC,NR,[]]|Pregs],CP,CR], IDP, Fecha, Cont, Etiq, NCR, [[],LU,[[IDP,A,F,C,LE,E,NV,VF,VC,REC,NR,[[CR,NUA,Fecha,Cont,Etiq,"",0,0,0]]]|Pregs],CP,NCR]):- !.
agregarResPreg([[NUA,CU,RU,REP],LU,[[IDP,A,F,C,LE,E,NV,VF,VC,REC,NR,[Res|Resps]]|Pregs],CP,CR], IDP, Fecha, Cont, Etiq, NCR, [[],LU,[[IDP,A,F,C,LE,E,NV,VF,VC,REC,NR,[[CR,NUA,Fecha,Cont,Etiq,"",0,0,0],Res|Resps]]|Pregs],CP,NCR]):- !.
agregarResPreg([UA,LU,[Preg|Pregs],CP,CR], IDP, Fecha, Cont, Etiq, NCR, [NewUA,LU,[Preg|NewPregs],CP,NCR]):- agregarResPreg([UA,LU,Pregs,CP,CR], IDP, Fecha, Cont, Etiq, NCR, [NewUA,LU,NewPregs,CP,NCR]).


noExisteUsuarioActivo([]):- !.

%predicado para saber si existe una pregunta:
%Utiliza el operador de corte para confirmar la existencia de una pregunta según su id en la lista de preguntas.
existePregEnStack([UA,LU,[[IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,RESPUESTAS]|_],CP,CR], IDP):- !.
existePregEnStack([UA,LU,[_|Preguntas],CP,CR], IDP):-
existePregEnStack([UA,LU,Preguntas,CP,CR], IDP).


answer([UA,LU,LP,CP,CR], Fecha, IDP, Contenido, ListEtiq, Stack2):-
esStack([UA,LU,LP,CP,CR]), esFecha(Fecha), integer(IDP), string(Contenido), esListaString(ListEtiq), 
existePregEnStack([UA,LU,LP,CP,CR],IDP), not(noExisteUsuarioActivo(UA)), NCR is CR + 1, 
agregarResPreg([UA,LU,LP,CP,CR], IDP, Fecha, Contenido, ListEtiq, NCR, Stack2).


%8Desarrollo predicado accept:


siExisteResAccept([[IDR, AutorRes, FP, C, LE, _, VF, VC, NR]|Respuestas], IDR, AutorRes, [[IDR, AutorRes, FP, C, LE, "Aceptada", VF, VC, NR]|Respuestas]):- !.
siExisteResAccept([Respuesta|Respuestas], IDR, AutorRes, [Respuesta|NewRespuestas]):- siExisteResAccept(Respuestas, IDR, AutorRes, NewRespuestas).

siExistePregDeUserActivoYResAccept([[IDP,AP,FP,C,LE,EP,NV,VF,VC,[_,Monto],NR,Respuestas]|Preguntas], IDP, AP, IDR, AutorRes, Monto, NewPreg):-
siExisteResAccept(Respuestas, IDR, Autor, NRespuestas), AutorRes = Autor, NewPreg = [[IDP,AP,FP,C,LE,EP,NV,VF,VC,["",0],NR,NRespuestas]|Preguntas], !.

siExistePregDeUserActivoYResAccept([Pregunta|Preguntas], IDP, AP, IDR, AutorRes, Monto, [Pregunta|NewPreguntas]):- 
siExistePregDeUserActivoYResAccept(Preguntas, IDP, AP, IDR, AutorRes, Monto, NewPreguntas).

actualizarRepUser([[NameUser,CU,R,LR]|Usuarios], NameUser, Monto, [[NameUser,CU,NewRep,LR]|Usuarios]):- NewRep is R + Monto.
actualizarRepUser([Usuario|Usuarios], NameUser, Monto, [Usuario|NewUsuarios]):- actualizarRepUser(Usuarios, NameUser, Monto, NewUsuarios).


accept([[NomUserActivo,CUA,RUA,REP], ListUser, ListPreg, CorrPreg, CorrRes], IdPreg, IdRes, Stack2):-
esStack([[NomUserActivo,CUA,RUA,REP], ListUser, ListPreg, CorrPreg, CorrRes]), integer(IdPreg), integer(IdRes),
siExistePregDeUserActivoYResAccept(ListPreg, IdPreg, NomUserActivo, IdRes, AutorRes, MontoRecom, NewListPreg), M is MontoRecom + 15,
actualizarRepUser(ListUser, NomUserActivo, 2, ListUser2), actualizarRepUser(ListUser2, AutorRes, M, ListUser3),
Stack2 = [[], ListUser3, NewListPreg, CorrPreg, CorrRes].



%Desarrollo predicado stackToString:
listToString([],".").
listToString([H|T],Final):- 
atomics_to_string([" ",H, " -"], Str), listToString(T,Final2), string_concat(Str, Final2, Final).

listUserToString([],"\n\n").
listUserToString([[NameUser, Pass, Reputacion, Referencias]|Usuarios], StringFinal):- esUsuario([NameUser, Pass, Reputacion, Referencias]),
listToString(Referencias,Ref),
atomics_to_string(["Nombre Usuario: ",NameUser, "\nClave: ",Pass, "\nReputacion: ",Reputacion, "\nReferencias: ",Ref, "\n\n"], Str),
listUserToString(Usuarios, StringFinal2), string_concat(Str, StringFinal2, StringFinal).

listResToString([], "\n\n").
listResToString([[IDR, AR, [D,M,A], C, Etiq, EA, VF, VC, NR]|Respuestas], StringRes):- esRespuesta([IDR, AR, [D,M,A], C, Etiq, EA, VF, VC, NR]),
listToString(Etiq,E), atomics_to_string([D,"/",M,"/",A],FP),
atomics_to_string(["ID Respuesta: ",IDR, "\nAutor: ",AR, "\nFecha: ",FP , "\nContenido: ",C, "\nEtiquetas: ",E, "\nEstado: ",EA, "\nVotos a favor: ",VF, "\nVotos en contra: ",VC, "\nReportes: ",NR, "\n\n"], Str),
listResToString(Respuestas, StringRes2), string_concat(Str, StringRes2, StringRes).

listPregToString([], "\n").
listPregToString([[IDP,AP,[D,M,A],C,LE,EP,NV,VF,VC,[Ofertor,Monto],NR,Respuestas]|Preguntas],StringPreg):- esPregunta([IDP,AP,[D,M,A],C,LE,EP,NV,VF,VC,[Ofertor,Monto],NR,Respuestas]),
listToString(LE,E), atomics_to_string([D,"/",M,"/",A],FP), atomics_to_string([Ofertor, " ofrece ", Monto, " puntos "],R), listResToString(Respuestas,Res),
atomics_to_string(["ID Pregunta: ",IDP, "\nAutor: ",AP, "\nFecha: ",FP, "\nContenido: ",C, "\nEtiquetas: ",E, "\nEstado: ",EP, "\nVisualizaciones: ",NV,"\nVotos a favor: ",VF, "\nVotos en contra: ",VC,"\nRecompensa: ",R, "\nReportes: ",NR,"\nRespuestas:\n\n",Res,"\n\n"], Str),
listPregToString(Preguntas, StringPreg2), string_concat(Str, StringPreg2, StringPreg).

allPregUsuario([],_,[]).
allPregUsuario([[IDP,NameUser,FP,C,LE,EP,NV,VF,VC,R,NR,Res]|Pregs], NameUser, [[IDP,NameUser,FP,C,LE,EP,NV,VF,VC,R,NR,Res]|NewPreg]):- allPregUsuario(Pregs, NameUser, NewPreg).
allPregUsuario([Preg|Pregs], NameUser, NewPregs):- allPregUsuario(Pregs, NameUser, NewPregs).

stackToString([[], LU, LP, CP, CR], StackStr):- esStack([[], LU, LP, CP, CR]),
listUserToString(LU,LUStr), listPregToString(LP,LPStr), atomics_to_string(["StackOverflow:\n\n", "Usuarios:\n",LUStr, "Preguntas:\n",LPStr], StackStr).

stackToString([[NUA,CU,RU,REP], LU, LP, CP, CR], StackStr):- esStack([[NUA,CU,RU,REP], LU, LP, CP, CR]),
listUserToString([[NUA,CU,RU,REP]],UAStr),allPregUsuario(LP,NUA,NLP), listPregToString(NLP,LPStr),
atomics_to_string(["\nUsuario Activo:\n",UAStr, "Preguntas usuario:\n",LPStr], StackStr).


%Desarrollo predicado vote:


getQuestion([UA,LU,[[IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,Respuestas]|_],CP,CR], IDP, [IDP,AP,FP,C,LE,EP,NV,VF,VC,REC,NR,Respuestas]):- !.
getQuestion([UA,LU,[_|Preguntas],CP,CR], IDP, Pregunta):- getQuestion([UA,LU,Preguntas,CP,CR], IDP, Pregunta).

tomarRespuesta([[IDR,AR,FP,C,LE,EA,VF,VC,NR]|_], IDR, [IDR,AR,FP,C,LE,EA,VF,VC,NR]):- !.
tomarRespuesta([_|Respuestas], IDR, Respuesta):- tomarRespuesta(Respuestas, IDR, Respuesta).

getAnswer([UA,LU,[[IDP,A,F,C,LE,E,NV,VF,VC,REC,NR,Respuestas]|Pregs],CP,CR], IDP, IDR, Respuesta):- tomarRespuesta(Respuestas, IDR, Respuesta).
getAnswer([UA,LU,[_|Preguntas],CP,CR], IDP, IDR, Respuesta):- getAnswer([UA,LU,Preguntas,CP,CR], IDP, IDR, Respuesta).


voteRes([[IDR,AR,FP,C,LE,EA,VF,VC,NR]|Resps], LU, [IDR,AR,FP,C,LE,EA,VF,VC,NR], true, [[IDR,AR,FP,C,LE,EA,NVF,NVC,NR]|Resps], NLU):-
NVF is VF + 1, NVC is VC, actualizarRepUser(LU, AR, 10, NLU).
voteRes([[IDR,AR,FP,C,LE,EA,VF,VC,NR]|Resps], LU, [IDR,AR,FP,C,LE,EA,VF,VC,NR], false, [[IDR,AR,FP,C,LE,EA,NVF,NVC,NR]|Resps], NLU):-
NVC is VC + 1, NVF is VF, actualizarRepUser(LU, NUA, -2, NLU).
voteRes([Res|Resps], LU, Respuesta, Booleano, [Res|NewResps], NLU):- voteRes(Resps, LU, Respuesta, Booleano, NewResps, NewLU).


vote([[NUA,CU,RU,REP],LU,[[IDP,NUA,F,C,LE,E,NV,VF,VC,REC,NR,Resps]|Pregs],CP,CR], [IDP,NUA,F,C,LE,E,NV,VF,VC,REC,NR,Resps], true, [[],NLU,[[IDP,NUA,F,C,LE,E,NV,NVF,NVC,REC,NR,Resps]|Pregs],CP,CR]):- 
NVF is VF + 1, NVC is VC, actualizarRepUser(LU, NUA, 10, NLU).

vote([[NUA,CU,RU,REP],LU,[[IDP,NUA,F,C,LE,E,NV,VF,VC,REC,NR,Resps]|Pregs],CP,CR], [IDP,NUA,F,C,LE,E,NV,VF,VC,REC,NR,Resps], false, [[],NLU,[[IDP,NUA,F,C,LE,E,NV,NVF,NVC,REC,NR,Resps]|Pregs],CP,CR]):- 
NVC is VC + 1, NVF is VF, actualizarRepUser(LU, NUA, -2, NLU).

vote([UA,LU,[Preg|Pregs],CP,CR], [IDP,NUA,F,C,LE,E,NV,VF,VC,REC,NR,Resps], Booleano, [NewUA,NewLU,[Preg|NewPregs],CP,CR]):- 
vote([UA,LU,Pregs,CP,CR], [IDP,NUA,F,C,LE,E,NV,VF,VC,REC,NR,Resps], Booleano, [NewUA,NewLU,NewPregs,CP,CR]).


vote([[NUA,CU,RU,REP],LU,[[IDP,AP,F,C,LE,E,NV,VF,VC,REC,NR,Resps]|Pregs],CP,CR], [IDR,AR,FPR,CR,LER,ER,VFR,VCR,NRR], Booleano, [[],NLU,[[IDP,AP,F,C,LE,E,NV,VF,VC,REC,NR,NewResps]|Pregs],CP,CR]):-
voteRes(Resps, LU, [IDR,AR,FPR,CR,LER,ER,VFR,VCR,NRR], Booleano, NewResps, NLU), !.

vote():-
vote([[NUA,CU,RU,REP],LU,Pregs,CP,CR],[IDR,AR,FPR,CR,LER,ER,VFR,VCR,NRR], Booleano, [[],NewLU,Pregs,CP,CR]).


%Arreglar acept, vote , constructires.





