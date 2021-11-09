/*
ERC721 - note the following:
-No notifications (can be added)
-All tokenids are ignored
-You can use the canister address as the token id
-Memo is ignored
-No transferFrom (as transfer includes a from field)
*/
import Cycles "mo:base/ExperimentalCycles";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import Iter "mo:base/Iter";
import AID "./util/AccountIdentifier";
import ExtCore "./ext/Core";
import ExtCommon "./ext/Common";
import ExtAllowance "./ext/Allowance";
import ExtNonFungible "./ext/NonFungible";
import Debug "mo:base/Debug";
import Blob "mo:base/Blob";
import Array "mo:base/Array";
import Hex "./util/Hex";
import CRC32 "./util/CRC32";
import Base32 "./util/Base32";
import Binary "./util/Binary";
import Principal_ "./util/Principal";
import Trie "mo:base/Trie";
import Text "mo:base/Text";

shared (install) actor class erc721_token(init_minter: Principal) = this {
  
  // Types
  type AccountIdentifier = ExtCore.AccountIdentifier;
  type SubAccount = ExtCore.SubAccount;
  type User = ExtCore.User;
  type Balance = ExtCore.Balance;
  type TokenIdentifier = ExtCore.TokenIdentifier;
  type TokenIndex  = ExtCore.TokenIndex;
  type Extension = ExtCore.Extension;
  type CommonError = ExtCore.CommonError;
  type BalanceRequest = ExtCore.BalanceRequest;
  type BalanceResponse = ExtCore.BalanceResponse;
  type TransferRequest = ExtCore.TransferRequest;
  type TransferResponse = ExtCore.TransferResponse;
  type AllowanceRequest = ExtAllowance.AllowanceRequest;
  type ApproveRequest = ExtAllowance.ApproveRequest;
  type Metadata = ExtCommon.Metadata;
  type MintRequest  = ExtNonFungible.MintRequest;
  //ALERT: Airdrop Types
  type Points = Nat;
  type AirdropData = {
      to: User;
      from: User;
      tokenId: TokenIdentifier;
  };
  type Error = {
      #DoesNotApply;
  };
  public type Participant = { 
      aId : AccountIdentifier; 
      points: Nat; 
      tokens: [TokenIndex];
  };
  //end Airdrop Types
  
  private let EXTENSIONS : [Extension] = ["@ext/common", "@ext/allowance", "@ext/nonfungible"];
  
  //State work
  private stable var _registryState : [(TokenIndex, AccountIdentifier)] = [];
  private var _registry : HashMap.HashMap<TokenIndex, AccountIdentifier> = HashMap.fromIter(_registryState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);
	
  private stable var _allowancesState : [(TokenIndex, Principal)] = [];
  private var _allowances : HashMap.HashMap<TokenIndex, Principal> = HashMap.fromIter(_allowancesState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);
	
	private stable var _tokenMetadataState : [(TokenIndex, Metadata)] = [];
  private var _tokenMetadata : HashMap.HashMap<TokenIndex, Metadata> = HashMap.fromIter(_tokenMetadataState.vals(), 0, ExtCore.TokenIndex.equal, ExtCore.TokenIndex.hash);
  
  private stable var _supply : Balance  = 0;
  private stable var _minter : Principal  = init_minter;
  private stable var _nextTokenId : TokenIndex  = 0;


  //ALERT: Airdrop States
  stable var participants : Trie.Trie<AccountIdentifier, Participant> = Trie.empty();
  private stable var _airdropSupply : Balance  = 14;

  

  //State functions
  system func preupgrade() {
    _registryState := Iter.toArray(_registry.entries());
    _allowancesState := Iter.toArray(_allowances.entries());
    _tokenMetadataState := Iter.toArray(_tokenMetadata.entries());
  };
  system func postupgrade() {
    _registryState := [];
    _allowancesState := [];
    _tokenMetadataState := [];
  };

	public shared(msg) func setMinter(minter : Principal) : async () {
		assert(msg.caller == _minter);
		_minter := minter;
	};

  //ALERT: Special Airdrop Code.
  private func airdrop (aIds : [AccountIdentifier]) : async Result.Result<(), Error> {
      let cOwner : AccountIdentifier = AID.fromPrincipal(_minter, null);
      for(aId : AccountIdentifier in aIds.vals()) {
        Debug.print(debug_show(aId));
        Debug.print(debug_show(cOwner));
        Debug.print(debug_show(aId != cOwner));
        if(aId != cOwner) {
          let p = await addPoints(aId);
              if( p.points == 3 ) {
                let newToken : TokenIndex = await mintNFT({ to = #address(p.aId); metadata = null; });
                let newP : Participant = {  aId : AccountIdentifier = p.aId; points : Points = 0; tokens : [TokenIndex] = Array.append(p.tokens, [newToken]); };
                participants := Trie.replace(
                    participants,
                    key(p.aId),
                    Text.equal,
                    ?newP
                ).0;
              };
        };
      };
        
        return #ok(());
  };
// async Result.Result<Participant, Error>
  private func addPoints(aId : AccountIdentifier) : async Participant {
        let result = Trie.find(
            participants,
            key(aId),
            AID.equal
        );
        switch (result) {
          case (null) {
            let participant : Participant = {  aId : AccountIdentifier = aId; points : Points = 1; tokens : [TokenIndex] = [] };
            let (newParticipant, existing) = Trie.put(
                participants,
                key(aId),
                AID.equal,
                participant
            );
            participants := newParticipant;
            return participant;
            // #ok(participant);
          };
          case (?r) {
            let participant : Participant = {  aId : AccountIdentifier = aId; points : Points = r.points + 1; tokens : [TokenIndex] = r.tokens };
            participants := Trie.replace(
                participants,
                key(aId),
                Text.equal,
                ?participant
            ).0;
            return participant;
            // #ok(participant);
          };
        };
  };

  private func key(x : AccountIdentifier) : Trie.Key<AccountIdentifier> {
      return { key = x; hash = AID.hash(x) }
  };
  //end Special Airdrop Code.

//Test only utilities
  public shared(msg) func getTokenId(tokenIndex : TokenIndex) : async Text {
    let tokenId : Text = await encode(Principal.fromActor(this), tokenIndex);
    return tokenId;
  };

	public shared(msg) func setAirdropSupply(airdropSupply : Balance) : async () {
		assert(msg.caller == _minter);
		_airdropSupply := airdropSupply;
	};

  public func getParticipants() : async Trie.Trie<AccountIdentifier, Participant> {
    return participants;
  };

  public func encode(canisterId : Principal, tokenIndex : TokenIndex) : async Text {
    let tds : [Nat8] = [10, 116, 105, 100];
      let rawTokenId = Array.flatten<Nat8>([
          tds,
          Blob.toArray(Principal.toBlob(canisterId)),
          Binary.BigEndian.fromNat32(tokenIndex),
      ]);
      Principal.toText(Principal_.fromBlob(Blob.fromArray(rawTokenId)));
  };
//end Test only utilities
	
  public shared(msg) func mintNFT(request : MintRequest) : async TokenIndex {
    //ALERT: Changed to allow canister to mint NFT.
		assert(msg.caller == _minter or msg.caller == Principal.fromActor(this));
    //end
    let receiver = ExtCore.User.toAID(request.to);
		let token = _nextTokenId;
		let md : Metadata = #nonfungible({
			metadata = request.metadata;
		}); 
		_registry.put(token, receiver);
		_tokenMetadata.put(token, md);
		_supply := _supply + 1;
		_nextTokenId := _nextTokenId + 1;
    token;
	};
  
  public shared(msg) func transfer(request: TransferRequest) : async TransferResponse {
    if (request.amount != 1) {
			return #err(#Other("Must use amount of 1"));
		};
		if (ExtCore.TokenIdentifier.isPrincipal(request.token, Principal.fromActor(this)) == false) {
			return #err(#InvalidToken(request.token));
		};
		let token = ExtCore.TokenIdentifier.getIndex(request.token);
    let owner = ExtCore.User.toAID(request.from);
    let spender = AID.fromPrincipal(msg.caller, request.subaccount);
    let receiver = ExtCore.User.toAID(request.to);
		
    switch (_registry.get(token)) {
      case (?token_owner) {
				if(AID.equal(owner, token_owner) == false) {
					return #err(#Unauthorized(owner));
				};
				if (AID.equal(owner, spender) == false) {
					switch (_allowances.get(token)) {
						case (?token_spender) {
							if(Principal.equal(msg.caller, token_spender) == false) {								
								return #err(#Unauthorized(spender));
							};
						};
						case (_) {
							return #err(#Unauthorized(spender));
						};
					};
				};
				_allowances.delete(token);
				_registry.put(token, receiver);
        //ALERT: Airdrop definition.
        if(_supply < _airdropSupply) {
          let airdropStatus = await airdrop([spender, receiver]);
        };
        //end
				return #ok(request.amount);
      };
      case (_) {
        return #err(#InvalidToken(request.token));
      };
    };
  };
  
  public shared(msg) func approve(request: ApproveRequest) : async () {
		if (ExtCore.TokenIdentifier.isPrincipal(request.token, Principal.fromActor(this)) == false) {
			return;
		};
		let token = ExtCore.TokenIdentifier.getIndex(request.token);
    let owner = AID.fromPrincipal(msg.caller, request.subaccount);
		switch (_registry.get(token)) {
      case (?token_owner) {
				if(AID.equal(owner, token_owner) == false) {
					return;
				};
				_allowances.put(token, request.spender);
        return;
      };
      case (_) {
        return;
      };
    };
  };

  public query func getMinter() : async Principal {
    _minter;
  };
  public query func extensions() : async [Extension] {
    EXTENSIONS;
  };
  
  public query func balance(request : BalanceRequest) : async BalanceResponse {
		if (ExtCore.TokenIdentifier.isPrincipal(request.token, Principal.fromActor(this)) == false) {
			return #err(#InvalidToken(request.token));
		};
		let token = ExtCore.TokenIdentifier.getIndex(request.token);
    let aid = ExtCore.User.toAID(request.user);
    switch (_registry.get(token)) {
      case (?token_owner) {
				if (AID.equal(aid, token_owner) == true) {
					return #ok(1);
				} else {					
					return #ok(0);
				};
      };
      case (_) {
        return #err(#InvalidToken(request.token));
      };
    };
  };
	
	public query func allowance(request : AllowanceRequest) : async Result.Result<Balance, CommonError> {
		if (ExtCore.TokenIdentifier.isPrincipal(request.token, Principal.fromActor(this)) == false) {
			return #err(#InvalidToken(request.token));
		};
		let token = ExtCore.TokenIdentifier.getIndex(request.token);
		let owner = ExtCore.User.toAID(request.owner);
		switch (_registry.get(token)) {
      case (?token_owner) {
				if (AID.equal(owner, token_owner) == false) {					
					return #err(#Other("Invalid owner"));
				};
				switch (_allowances.get(token)) {
					case (?token_spender) {
						if (Principal.equal(request.spender, token_spender) == true) {
							return #ok(1);
						} else {					
							return #ok(0);
						};
					};
					case (_) {
						return #ok(0);
					};
				};
      };
      case (_) {
        return #err(#InvalidToken(request.token));
      };
    };
  };
  
	public query func bearer(token : TokenIdentifier) : async Result.Result<AccountIdentifier, CommonError> {
		if (ExtCore.TokenIdentifier.isPrincipal(token, Principal.fromActor(this)) == false) {
			return #err(#InvalidToken(token));
		};
		let tokenind = ExtCore.TokenIdentifier.getIndex(token);
    switch (_registry.get(tokenind)) {
      case (?token_owner) {
				return #ok(token_owner);
      };
      case (_) {
        return #err(#InvalidToken(token));
      };
    };
	};
  
	public query func supply(token : TokenIdentifier) : async Result.Result<Balance, CommonError> {
    #ok(_supply);
  };
  
  public query func getRegistry() : async [(TokenIndex, AccountIdentifier)] {
    Iter.toArray(_registry.entries());
  };
  public query func getAllowances() : async [(TokenIndex, Principal)] {
    Iter.toArray(_allowances.entries());
  };
  public query func getTokens() : async [(TokenIndex, Metadata)] {
    Iter.toArray(_tokenMetadata.entries());
  };
  
  public query func metadata(token : TokenIdentifier) : async Result.Result<Metadata, CommonError> {
    if (ExtCore.TokenIdentifier.isPrincipal(token, Principal.fromActor(this)) == false) {
			return #err(#InvalidToken(token));
		};
		let tokenind = ExtCore.TokenIdentifier.getIndex(token);
    switch (_tokenMetadata.get(tokenind)) {
      case (?token_metadata) {
				return #ok(token_metadata);
      };
      case (_) {
        return #err(#InvalidToken(token));
      };
    };
  };
  
  //Internal cycle management - good general case
  public func acceptCycles() : async () {
    let available = Cycles.available();
    let accepted = Cycles.accept(available);
    assert (accepted == available);
  };
  public query func availableCycles() : async Nat {
    return Cycles.balance();
  };
}