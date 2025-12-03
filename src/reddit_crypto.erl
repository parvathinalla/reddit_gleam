-module(reddit_crypto).
-export([
  generate_rsa_keypair/0,
  sign_message/2,
  verify_signature/3,
  public_key_to_string/1,
  string_to_public_key/1,
  private_key_to_string/1,
  string_to_private_key/1
]).

%% ============================================================================
%% RSA-2048 Key Generation
%% ============================================================================

generate_rsa_keypair() ->
  % Generate RSA-2048 key pair
  % Returns {PublicKey, PrivateKey} as Erlang terms
  crypto:generate_key(rsa, {2048, 65537}).

%% ============================================================================
%% Digital Signature Operations
%% ============================================================================

sign_message(Message, PrivateKeyBin) when is_binary(Message) ->
  sign_message(binary_to_list(Message), PrivateKeyBin);

sign_message(Message, PrivateKeyBin) when is_list(Message) ->
  try
    % Decode private key
    PrivateKey = string_to_private_key(PrivateKeyBin),

    % Create hash of message
    Hash = crypto:hash(sha256, Message),

    % Sign the hash with private key
    Signature = public_key:sign(Hash, sha256, PrivateKey),

    % Encode signature to base64 for storage
    base64:encode(Signature)
  catch
    Error:Reason ->
      io:format("Signing error: ~p:~p~n", [Error, Reason]),
      {error, signing_failed}
  end.

verify_signature(Message, SignatureBase64, PublicKeyBin) when is_binary(Message) ->
  verify_signature(binary_to_list(Message), SignatureBase64, PublicKeyBin);

verify_signature(Message, SignatureBase64, PublicKeyBin) when is_list(Message) ->
  try
    % Decode signature from base64
    Signature = base64:decode(SignatureBase64),

    % Decode public key
    PublicKey = string_to_public_key(PublicKeyBin),

    % Create hash of message
    Hash = crypto:hash(sha256, Message),

    % Verify signature
    public_key:verify(Hash, sha256, Signature, PublicKey)
  catch
    Error:Reason ->
      io:format("Verification error: ~p:~p~n", [Error, Reason]),
      false
  end.

%% ============================================================================
%% Key Encoding/Decoding (for storage and transmission)
%% ============================================================================

public_key_to_string(PublicKey) ->
  % Encode public key to base64 for storage
  base64:encode(term_to_binary(PublicKey)).

string_to_public_key(EncodedKey) when is_binary(EncodedKey) ->
  string_to_public_key(binary_to_list(EncodedKey));

string_to_public_key(EncodedKey) when is_list(EncodedKey) ->
  try
    Decoded = base64:decode(EncodedKey),
    binary_to_term(Decoded)
  catch
    Error:Reason ->
      io:format("Public key decode error: ~p:~p~n", [Error, Reason]),
      throw({invalid_public_key, EncodedKey})
  end.

private_key_to_string(PrivateKey) ->
  % Encode private key to base64 (for storage)
  % WARNING: In production, encrypt this!
  base64:encode(term_to_binary(PrivateKey)).

string_to_private_key(EncodedKey) when is_binary(EncodedKey) ->
  string_to_private_key(binary_to_list(EncodedKey));

string_to_private_key(EncodedKey) when is_list(EncodedKey) ->
  try
    Decoded = base64:decode(EncodedKey),
    binary_to_term(Decoded)
  catch
    Error:Reason ->
      io:format("Private key decode error: ~p:~p~n", [Error, Reason]),
      throw({invalid_private_key, EncodedKey})
  end.